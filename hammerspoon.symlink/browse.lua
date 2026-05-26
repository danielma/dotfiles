hs.loadSpoon("URLDispatcher")

spoon.URLDispatcher.url_patterns = {
  { "asana.com", "com.apple.Safari.WebApp.3F333E75-B8FB-4E79-97D8-F7DCB3221FAB" },
  { ".*", "company.thebrowser.Browser" },
}

-- Start the dispatcher
spoon.URLDispatcher:start()
