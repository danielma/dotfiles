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
   {'1', '1Password'},
   {'a', 'Asana'},
   {'b', 'Basecamp 3'},
   {'d', 'Dash'},
   {'e', 'Emacs'},
   {'c', launchBrowser},
   {'f', 'Safari'},
   {'h', '2Do'},
   {'i', 'Spotify'},
   {'l', 'BusyCal'},
   {'s', 'Slack'},
   {'t', 'Alacritty'},
   {'r', 'Trello'},
   {'m', 'Messages'},
   {'n', 'Notion'},
   {'p', 'Paw'},
   {'q', 'TablePlus'},
   {'v', 'Visual Studio Code'},
   {'w', 'iA Writer'},
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
