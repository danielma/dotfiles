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

  static let file = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent(".sptf-creds-v2.json")

  static func load() -> Credentials {
    let fileData = try! Data(contentsOf: Credentials.file)
    return try! JSONDecoder().decode(Credentials.self, from: fileData)
  }

  static func updateAccessToken(_ newToken: String) -> Credentials {
    var existingCredentials = Credentials.load()
    existingCredentials.accessToken = newToken

    let json = try! JSONEncoder().encode(existingCredentials)
    let output = String(data: json, encoding: .ascii)!

    try! output.write(to: Credentials.file, atomically: true, encoding: .ascii)

    return existingCredentials
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

struct Spotify {
  private static func realApiRequest(_ route: String, method: HTTPMethod? = nil, body: Data? = nil) -> JSONResponse {
    return Utils.synchronousRequest(
      "https://api.spotify.com/v1/\(route)",
      headers: ["Authorization": "Bearer \(Credentials.load().accessToken)"],
      method: method,
      body: body
    )
  }

  private static func apiRequest(_ route: String, method: HTTPMethod? = nil, body: Data? = nil) -> JSONResponse {
    let initialResult = realApiRequest(route, method: method, body: body)

    if initialResult.0["error"] != nil {
      refreshTokenAndSave()
      return realApiRequest(route, method: method, body: body)
    } else {
      return initialResult
    }
  }

  private static func apiRequest(_ route: String, method: HTTPMethod? = nil, body: String? = nil) -> JSONResponse {
    return apiRequest(route, method: method, body: body?.data(using: .utf8))
  }

  private static func apiRequest(_ route: String, method: HTTPMethod? = nil) -> JSONResponse {
    let thisVarHelpsAvoidAmbiguity: Data? = nil
    return apiRequest(route, method: method, body: thisVarHelpsAvoidAmbiguity)
  }

  private static func refreshToken() -> String {
    let credentials = Credentials.load()
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

  private static func refreshTokenAndSave() {
    let newToken = refreshToken()

    _ = Credentials.updateAccessToken(newToken)
  }

  static func currentlyPlaying() -> JSONResponse {
    return Spotify.apiRequest("me/player/currently-playing")
  }

  static func saveToLibrary() -> JSONResponse {
    let currentlyPlaying = Spotify.currentlyPlaying().json
    let item = currentlyPlaying["item"] as! [String: Any]
    let id = item["id"] as! String
    print(item["name"] as! String)
    let body = try! JSONEncoder().encode(["ids": [id]])
    return Spotify.apiRequest("me/tracks", method: .PUT, body: body)
  }
}

if CommandLine.arguments.count == 1 {
  print("Needs at least one argument")
  exit(1)
}

let command = CommandLine.arguments[1]

switch (command) {
case "info":
  dump(Spotify.currentlyPlaying().json)
case "save":
  let result = Spotify.saveToLibrary()
  if result.json.count > 0 {
    dump(result.json)
  } else {
    dump(result.response.statusCode)
  }
default:
  print("Unknown command: \(command)")
  exit(1)
}

