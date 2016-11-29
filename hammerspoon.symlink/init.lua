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
   {'d', 'DevDocs'},
   {'e', 'Emacs'},
   {'c', 'Google Chrome'},
   {'i', 'iTunes'},
   {'s', 'Slack'},
   {'t', 'iTerm'},
   {'r', 'Trello'},
   {'m', 'Messages'},
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
end

-- Exit launch mode with enter
appModal:bind(
  {},
  'return',
  function()
    appModal:exit()
  end
)
