#!/usr/bin/env xcrun swift
// -*- mode: swift -*-

import Foundation

enum TerminalStyle: String {
  case red = "31"
  case purple = "38;5;5"
  case darkGray = "90"
  case reset = "0"

  public func fullString() -> String {
    return "\u{001B}[\(rawValue)m"
  }
}

extension String {
  func style(_ style: TerminalStyle) -> String {
    let str = self.replacingOccurrences(of: TerminalStyle.reset.fullString(), with: TerminalStyle.reset.fullString() + style.fullString())

    return style.fullString() + str + TerminalStyle.reset.fullString()
  }
}

typealias JSONResponse = (json: [String: Any], response: HTTPURLResponse, error: Error?)

enum HTTPMethod: String {
  case POST
  case PUT
  case GET
}

struct Credentials: Codable {
  var refreshToken: String
  var accessToken: String
  var clientId: String
  var clientSecret: String
}

struct DB: Codable {
  var credentials: Credentials
  var userId: String
  var playlistIds: [String: String]

  static let file = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent(".sptf.json")

  static func load() -> DB {
    let fileData = try! Data(contentsOf: DB.file)
    return try! JSONDecoder().decode(DB.self, from: fileData)
  }

  private mutating func save() {
    let json = try! JSONEncoder().encode(self)
    let output = String(data: json, encoding: .ascii)!

    try! output.write(to: DB.file, atomically: true, encoding: .ascii)
  }

  mutating func updateCredentials(_ newCredentials: Credentials) {
    credentials = newCredentials
    save()
  }

  mutating func addPlaylistId(name: String, id: String) {
    playlistIds[name] = id
    save()
  }
}

struct Utils {
  static func b64(_ source: String) -> String {
    let encoded = source.data(using: .utf8)
    return encoded!.base64EncodedString()
  }

  static func synchronousRequest(_ urlString: String, headers: [String: String]? = nil, method: HTTPMethod? = nil, body: String? = nil) -> JSONResponse {
    return Utils.synchronousRequest(urlString, headers: headers, method: method, body: body?.data(using: .utf8))
  }

  static func synchronousRequest(_ urlString: String, headers maybeHeaders: [String: String]? = nil, method maybeMethod: HTTPMethod? = nil, body: Data? = nil) -> JSONResponse {
    let method = maybeMethod ?? .GET
    let headers = maybeHeaders ?? [:]

    let url = URL(string: urlString)!
    var data: Data?
    var response: HTTPURLResponse!
    var error: Error?

    let semaphore = DispatchSemaphore(value: 0)

    var request = URLRequest(url: url)
    request.httpMethod = method.rawValue
    if let body = body {
      request.httpBody = body
    }
    request.addValue("application/json", forHTTPHeaderField: "Accept")
    headers.forEach({ (tuple) in
      request.addValue(tuple.value, forHTTPHeaderField: tuple.key)
    })

    if let body = body {
      let stringBody = String(data: body, encoding: .ascii)
      print("\(method.rawValue.style(.purple)) \(urlString) \(stringBody ?? "")".style(.darkGray))
    } else {
      print("\(method.rawValue.style(.purple)) \(urlString)".style(.darkGray))
    }

    let task = URLSession.shared.dataTask(with: request) { (taskData, taskResponse, taskError) in
      data = taskData
      response = taskResponse as! HTTPURLResponse
      error = taskError

      semaphore.signal()
    }

    task.resume()

    _ = semaphore.wait(timeout: .distantFuture)

    if let taskData = data {
      if taskData.count > 0 {
        let jsonSerialized = try! JSONSerialization.jsonObject(with: taskData, options: []) as! [String: Any]

        return (jsonSerialized, response, error)
      } else {
        return ([:], response, error)
      }
    } else {
      fatalError("I couldn't do it")
    }
  }
}

class Spotify {
  struct Track {
    var id: String
    var name: String

    var uri: String {
      return "spotify:track:\(id)"
    }
  }

  var db: DB

  var credentials: Credentials {
    return db.credentials
  }

  init() {
    self.db = DB.load()
  }

  private func realApiRequest(_ route: String, method: HTTPMethod? = nil, body: Data? = nil) -> JSONResponse {
    return Utils.synchronousRequest(
      "https://api.spotify.com/v1/\(route)",
      headers: ["Authorization": "Bearer \(credentials.accessToken)"],
      method: method,
      body: body
    )
  }

  private func apiRequest(_ route: String, method: HTTPMethod? = nil, body: Data? = nil) -> JSONResponse {
    let initialResult = realApiRequest(route, method: method, body: body)

    if initialResult.0["error"] != nil {
      refreshTokenAndSave()
      return realApiRequest(route, method: method, body: body)
    } else {
      return initialResult
    }
  }

  private func apiRequest(_ route: String, method: HTTPMethod? = nil, body: String? = nil) -> JSONResponse {
    return apiRequest(route, method: method, body: body?.data(using: .utf8))
  }

  private func apiRequest(_ route: String, method: HTTPMethod? = nil) -> JSONResponse {
    let thisVarHelpsAvoidAmbiguity: Data? = nil
    return apiRequest(route, method: method, body: thisVarHelpsAvoidAmbiguity)
  }

  private func refreshToken() -> String {
    let auth = Utils.b64("\(credentials.clientId):\(credentials.clientSecret)")
    let (json, response, _) = Utils.synchronousRequest(
      "https://accounts.spotify.com/api/token",
      headers: ["Authorization": "Basic \(auth)"],
      method: .POST,
      body: "grant_type=refresh_token&refresh_token=\(credentials.refreshToken)"
    )

    if let accessToken = json["access_token"] as? String {
      return accessToken
    } else {
      dump(response)
      fatalError("Couldn't get a new access token!")
    }
  }

  private func refreshTokenAndSave() {
    let newToken = refreshToken()
    var newCredentials = db.credentials
    newCredentials.accessToken = newToken

    db.updateCredentials(newCredentials)
  }

  func currentTrack() -> Track {
    let info = apiRequest("me/player/currently-playing").json
    let item = info["item"] as! [String: Any]

    return Track(id: item["id"] as! String, name: item["name"] as! String)
  }

  private func addToLibrary(_ track: Track) -> JSONResponse {
    let body = try! JSONEncoder().encode(["ids": [track.id]])
    return apiRequest("me/tracks", method: .PUT, body: body)
  }

  func saveToLibrary() -> JSONResponse {
    let track = currentTrack()
    print(track.name)
    return addToLibrary(track)
  }

  private var monthFormatter: DateFormatter = {
    let formatter = DateFormatter()
    formatter.dateFormat = "MMMM yyyy"
    return formatter
  }()

  typealias PlaylistId = String

  enum PlaylistType {
    case month
  }

  private func findOrCreatePlaylist(_ type: PlaylistType) -> PlaylistId {
    let playlistName = "\(monthFormatter.string(from: Date())) Tracks"

    if let playlistId = db.playlistIds[playlistName] {
      return playlistId
    } else {
      let body = try! JSONEncoder().encode(["name": playlistName])
      let createPlaylist = apiRequest("users/\(db.userId)/playlists", method: .POST, body: body)
      let id = createPlaylist.json["id"] as! String

      db.addPlaylistId(name: playlistName, id: id)

      return id
    }
  }

  func saveToList() -> JSONResponse {
    let track = currentTrack()
    let trackUri = track.uri
    let result = addToLibrary(track)

    guard result.response.statusCode == 200 else { fatalError("Couldn't save \(track.id) to library") }

    print(track.name)

    let playlistId = findOrCreatePlaylist(.month)
    let playlistTracksUrl = "users/\(db.userId)/playlists/\(playlistId)/tracks"

    return apiRequest(
      playlistTracksUrl,
      method: .POST,
      body: try! JSONEncoder().encode(["uris": [trackUri]])
    )
  }
}

if CommandLine.arguments.count == 1 {
  print("Needs at least one argument")
  exit(1)
}

let command = CommandLine.arguments[1]
var spotify = Spotify()

switch (command) {
case "info":
  dump(spotify.currentTrack())
case "lib":
  let result = spotify.saveToLibrary()
  if result.json.count > 0 {
    dump(result.json)
  } else {
    dump(result.response.statusCode)
  }
case "save":
  let result = spotify.saveToList()
  dump(result.json)
  dump(result.response.statusCode)
default:
  print("Unknown command: \(command)")
  exit(1)
}

