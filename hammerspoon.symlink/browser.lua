browsers = { "Safari", "Firefox", "Google Chrome" }
lastBrowser = "Safari"

function launchBrowser()
   launch(lastBrowser)
end

function browserEvent(appName, eventType, app)
   if eventType ~= hs.application.watcher.activated then
      return
   end

   for _, browserName in pairs(browsers) do
      if browserName == appName then
         if appName ~= lastBrowser then
            print("changing browser from " .. lastBrowser .. " to " .. appName)
            lastBrowser = appName
         end

         break
      end
   end
end

browserWatcher = hs.application.watcher.new(browserEvent)
browserWatcher:start()
