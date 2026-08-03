hs.loadSpoon("URLDispatcher")

spoon.URLDispatcher.default_handler = "company.thebrowser.Browser"

spoon.URLDispatcher.url_patterns = {
  { "asana.com", "com.apple.Safari.WebApp.3F333E75-B8FB-4E79-97D8-F7DCB3221FAB" },
  { "^https?://linear%.app/planningcenter", "com.linear" }
}

-- Start the dispatcher
spoon.URLDispatcher:start()
