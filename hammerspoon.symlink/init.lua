-- Add unpack function
function unpack(table, index)
  index = index or 1
  if table[index] ~= nil then
    return table[index], unpack(table, index + 1)
  end
end

require "hyper-symbols"
require "window"
require "browser"

-- Sequential keybindings
appModal = hs.hotkey.modal.new({}, "F16")

enterAppMode = function()
   hyperModal.triggered = true
   hyperModal:exit()
   appModal:enter()
end

hyperModal:bind({}, 'return', enterAppMode)

-- Launch function
launch = function(appname)
  hs.application.launchOrFocus(appname)
end

-- Launch apps with Hyper-Enter
apps = {
   {'1', '1Password 6'},
   {'d', 'DevDocs'},
   {'e', 'Emacs'},
   {'c', launchBrowser},
   {'f', 'Safari'},
   {'g', '/Applications/Resolutions.app'},
   {'i', 'Spotify'},
   {'j', 'JQBX'},
   {'s', '/Applications/Slack.app'},
   {'t', 'iTerm'},
   {'k', 'Spark'},
   {'r', 'Trello'},
   {'m', 'Messages'},
   {'p', 'Postman'},
   {'q', 'Sequel Pro'},
   {'v', 'Visual Studio Code'},
   {'x', 'Xcode'},
}

for i, app in ipairs(apps) do
  appModal:bind(
    {},
    app[1],
    nil,
    function()
       if type(app[2]) == "string" then
          launch(app[2])
       else
          app[2]()
       end
       appModal:exit()
    end
  )

  hs.hotkey.bind(
     'cmd-ctrl-alt-shift',
     app[1],
     nil,
     function()
       if type(app[2]) == "string" then
          launch(app[2])
       else
          app[2]()
       end
     end
  )
end

-- Exit launch mode with escape
appModal:bind(
  {},
  'escape',
  function()
    appModal:exit()
  end
)
