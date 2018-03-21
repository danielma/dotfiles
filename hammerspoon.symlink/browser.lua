local browsers = { "Safari", "Firefox", "Google Chrome" }
local defaultBrowser = "Firefox"
local lastBrowser

function launchBrowser()
   if lastBrowser then
      launch(lastBrowser)
   else
      launch(defaultBrowser)
   end
end

function browserEvent(appName, eventType, app)
   if eventType ~= hs.application.watcher.activated then
      return
   end

   for _, browserName in pairs(browsers) do
      if browserName == appName then
         lastBrowser = appName

         break
      end
   end
end

local browserWatcher = hs.application.watcher.new(browserEvent)
browserWatcher:start()
