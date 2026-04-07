hs.loadSpoon("URLDispatcher")

spoon.URLDispatcher.url_patterns = {
  { "asana.com", "com.BZG.Asana" },
  { ".*", "company.thebrowser.Browser" },
}

-- Start the dispatcher
spoon.URLDispatcher:start()
