-- Add unpack function
function unpack(table, index)
  index = index or 1
  if table[index] ~= nil then
    return table[index], unpack(table, index + 1)
  end
end

require "hyper-symbols"
require "window"

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
   {'c', 'FirefoxDeveloperEdition'},
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
}

for i, app in ipairs(apps) do
  appModal:bind(
    {},
    app[1],
    nil,
    function()
      launch(app[2])
      appModal:exit()
    end
  )

  hs.hotkey.bind(
     'cmd-ctrl-alt-shift',
     app[1],
     nil,
     function()
        launch(app[2])
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
