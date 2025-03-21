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
    let str = self.replacingOccurrences(
      of: TerminalStyle.reset.fullString(),
      with: TerminalStyle.reset.fullString() + style.fullString())

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

  static let file = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent(
    ".sptf.json")

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

  static func synchronousRequest(
    _ url: URL, headers: [String: String]? = nil, method: HTTPMethod? = nil, body: String? = nil
  ) -> JSONResponse {
    return Utils.synchronousRequest(
      url, headers: headers, method: method, body: body?.data(using: .utf8))
  }

  static func synchronousRequest(
    _ url: URL, headers maybeHeaders: [String: String]? = nil,
    method maybeMethod: HTTPMethod? = nil, body: Data? = nil
  ) -> JSONResponse {
    let method = maybeMethod ?? .GET
    let headers = maybeHeaders ?? [:]

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
      print(
        "\(method.rawValue.style(.purple)) \(url.absoluteString) \(stringBody ?? "")".style(
          .darkGray))
    } else {
      print("\(method.rawValue.style(.purple)) \(url.absoluteString)".style(.darkGray))
    }

    let task = URLSession.shared.dataTask(with: request) { (taskData, taskResponse, taskError) in
      data = taskData
      response = (taskResponse as! HTTPURLResponse)
      error = taskError

      semaphore.signal()
    }

    task.resume()

    _ = semaphore.wait(timeout: .distantFuture)

    if let taskData = data {
      if taskData.count > 0 {
        let jsonSerialized =
          try! JSONSerialization.jsonObject(with: taskData, options: []) as! [String: Any]

        return (jsonSerialized, response, error)
      } else {
        return ([:], response, error)
      }
    } else {
      fatalError("I couldn't do it")
    }
  }
}

enum Weekday: Int {
  case Sunday = 1
  case Monday
  case Tuesday
  case Wednesday
  case Thursday
  case Friday
  case Saturday
}

class Spotify {
  struct Track {
    var id: String
    var name: String
    var artistName: String

    var uri: String {
      return "spotify:track:\(id)"
    }
  }

  typealias PlaylistId = String
  struct Playlist {
    let id: PlaylistId
    let name: String

    enum Kind {
      case month
      case week(Weekday)
    }

    static func from(_ dict: [String: Any]) -> Playlist {
      let id = dict["id"] as! String
      let name = dict["name"] as! String

      return Playlist(id: id, name: name)
    }
  }

  var db: DB

  var credentials: Credentials {
    return db.credentials
  }

  init() {
    self.db = DB.load()
  }

  private func realApiRequest(_ route: URL, method: HTTPMethod? = nil, body: Data? = nil)
    -> JSONResponse
  {
    var headers = ["Authorization": "Bearer \(credentials.accessToken)"]

    if body != nil {
      headers["Content-Type"] = "application/json"
    }

    return Utils.synchronousRequest(
      route,
      headers: headers,
      method: method,
      body: body
    )
  }

  private func apiRequest(_ route: URL, method: HTTPMethod? = nil, body: Data? = nil)
    -> JSONResponse
  {
    let initialResult = realApiRequest(route, method: method, body: body)

    if initialResult.0["error"] != nil {
      refreshTokenAndSave()
      return realApiRequest(route, method: method, body: body)
    } else {
      return initialResult
    }
  }

  private func apiRequest(_ route: String, method: HTTPMethod? = nil, body: Data? = nil)
    -> JSONResponse
  {
    return apiRequest(
      URL(string: "https://api.spotify.com/v1/\(route)")!, method: method, body: body)
  }

  private func apiRequest(_ route: String, method: HTTPMethod? = nil, body: String? = nil)
    -> JSONResponse
  {
    return apiRequest(route, method: method, body: body?.data(using: .utf8))
  }

  private func apiRequest(_ route: String, method: HTTPMethod? = nil) -> JSONResponse {
    let thisVarHelpsAvoidAmbiguity: Data? = nil
    return apiRequest(route, method: method, body: thisVarHelpsAvoidAmbiguity)
  }

  private func apiRequestWithAllPages(_ route: String) -> [JSONResponse] {
    var requests = [JSONResponse]()

    let firstRequest = apiRequest(route)
    requests.append(firstRequest)
    var nextUrl = firstRequest.json["next"] as? String

    while let str = nextUrl, let url = URL(string: str) {
      let request = apiRequest(url)

      requests.append(request)

      nextUrl = request.json["next"] as? String
    }

    return requests
  }

  private func refreshToken() -> String {
    let auth = Utils.b64("\(credentials.clientId):\(credentials.clientSecret)")
    let (json, response, _) = Utils.synchronousRequest(
      URL(string: "https://accounts.spotify.com/api/token")!,
      headers: ["Authorization": "Basic \(auth)"],
      method: .POST,
      body: "grant_type=refresh_token&refresh_token=\(credentials.refreshToken)"
    )

    if let accessToken = json["access_token"] as? String {
      return accessToken
    } else {
      dump(json)
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
    let artists = item["artists"] as! [[String: Any]]
    let artist = artists[0]

    return Track(
      id: item["id"] as! String,
      name: item["name"] as! String,
      artistName: artist["name"] as! String
    )
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

  private var weekFormatter: DateFormatter = {
    let formatter = DateFormatter()
    formatter.dateFormat = "MMM d"
    return formatter
  }()

  private func lookupPlaylistByName(_ name: String) -> PlaylistId? {
    let allPlaylists = getPlaylists()

    if let playlist = allPlaylists.first(where: { $0.name == name }) {
      return playlist.id
    }

    return nil
  }

  private func findOrCreatePlaylist(_ type: Playlist.Kind? = nil, name: String? = nil) -> Playlist {
    let playlistName: String = {
      switch type {
      case .month:
        return "\(monthFormatter.string(from: Date())) Tracks"
      case .week(let weekDay):
        let now = Date()

        let startDate = Calendar.current.nextDate(
          after: now, matching: DateComponents(hour: 0, weekday: weekDay.rawValue),
          matchingPolicy: .nextTime, direction: .backward)!
        let endDate = Calendar.current.date(byAdding: .day, value: 6, to: startDate)!
        return
          "\(weekFormatter.string(from: startDate))-\(weekFormatter.string(from: endDate)) Tracks"
      default:
        return name!
      }
    }()

    if let playlistId = db.playlistIds[playlistName] {
      return Playlist(id: playlistId, name: playlistName)
    }

    if let playlistId = lookupPlaylistByName(playlistName) {
      db.addPlaylistId(name: playlistName, id: playlistId)
      return Playlist(id: playlistId, name: playlistName)
    }

    let body = try! JSONEncoder().encode(["name": playlistName])
    let createPlaylist = apiRequest("users/\(db.userId)/playlists", method: .POST, body: body)
    let id = createPlaylist.json["id"] as! String

    db.addPlaylistId(name: playlistName, id: id)

    return Playlist(id: id, name: playlistName)
  }

  func addToPlaylist(track: Track, playlist: Playlist) -> JSONResponse {
    let playlistTracksUrl = "users/\(db.userId)/playlists/\(playlist.id)/tracks"

    return apiRequest(
      playlistTracksUrl,
      method: .POST,
      body: try! JSONEncoder().encode(["uris": [track.uri]])
    )
  }

  func saveToList() -> JSONResponse {
    let track = currentTrack()

    print("\(track.name) - \(track.artistName)")

    let result = addToLibrary(track)

    guard result.response.statusCode == 200 else {
      print(result.response)
      fatalError("Couldn't save \(track.id) to library")
    }

    let playlist = findOrCreatePlaylist(.month)

    return addToPlaylist(track: track, playlist: playlist)
  }

  func newsletter() -> JSONResponse {
    let track = currentTrack()
    let playlist = findOrCreatePlaylist(.week(.Friday))

    dump(track)

    return addToPlaylist(track: track, playlist: playlist)
  }

  func findPlaylist(_ id: String) -> [Track] {
    let response = apiRequest(
      "playlists/\(id)",
      method: .GET
    )

    let tracks = response.json["tracks"] as! [String: Any]
    let items = tracks["items"] as! [[String: Any]]

    return (items).map { i in
      let t = i["track"] as! [String: Any]
      let artists = t["artists"] as! [[String: Any]]
      let artistName = artists.map { $0["name"] as! String }.joined(separator: " & ")
      return Track(id: t["id"] as! String, name: t["name"] as! String, artistName: artistName)
    }
  }

  private func getPlaylists() -> [Playlist] {
    let requests = apiRequestWithAllPages("me/playlists")

    return requests.flatMap { response in
        (response.json["items"] as! [Any]).compactMap { something in
            if let source =  something as? [String: Any] {
                return Playlist.from(source)
            } else {
                return nil
            }
      }
    }
  }
}

if CommandLine.arguments.count == 1 {
  print("Needs at least one argument")
  exit(1)
}

let command = CommandLine.arguments[1]
var spotify = Spotify()

switch command {
case "info":
  dump(spotify.currentTrack())
case "lib":
  let result = spotify.saveToLibrary()
  if result.json.count > 0 {
    dump(result.json)
  } else {
    dump(result.response.statusCode)
  }
case "p", "playlist":
  let result = spotify.findPlaylist(CommandLine.arguments[2])
  print(
    result.enumerated().map { "\($0 + 1). \($1.name) — \($1.artistName)" }.joined(separator: "\n"))
case "s", "save":
  let result = spotify.saveToList()
  dump(result.response.statusCode)
  dump(result.json)
case "n", "newsletter":
  let result = spotify.newsletter()
  dump(result.response.statusCode)
  dump(result.json)
default:
  print("Unknown command: \(command)")
  exit(1)
}
