-- Add unpack function
function unpack(table, index)
  index = index or 1
  if table[index] ~= nil then
    return table[index], unpack(table, index + 1)
  end
end

-- A global variable for the Hyper Mode
k = hs.hotkey.modal.new({}, 'F17')

-- Enter Hyper Mode when F18 (Hyper) is pressed
pressedF18 = function()
  k.triggered = false
  k:enter()
end

-- Leave Hyper Mode when F18 (Hyper) is pressed,
--   send ESCAPE if no other keys are pressed.
releasedF18 = function()
  k:exit()
  if not k.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Bind the Hyper key
f18 = hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)

-- Symbols Layer
-- .-----------------------------------------------------------------------------------.
-- |      |      |      |      |      |      |      |      |      |      |      |      |
-- |------+------+------+------+------+------+------+------+------+------+------+------|
-- |      |  q   |  w   |  e   |  \   |  +   |  -   |  /   |  i   |  o   |  p   |      |
-- |------+------+------+------+------+-------------+------+------+------+------+------|
-- |      | ct-\ |  {   |  [   |  (   |  =   |  _   |  )   |  ]   |  }   |  >   |  `   |
-- |------+------+------+------+------+------|------+------+------+------+------+------|
-- |      |  z   |  x   |  %   |  <   |  b   |  |   |  >   |  ,   |  .   |  /   |      |
-- `-----------------------------------------------------------------------------------'

symbolsTable = {
  {'a', {{'ctrl'}, '\\'}},
  {'c', {{'shift'}, '5'}},
  {'s', {{'shift'}, '['}},
  {'d', {{}, '['}},
  {'f', {{'shift'}, '9'}},
  {'g', {{}, '='}},
  {'h', {{'shift'}, '-'}},
  {'j', {{'shift'}, '0'}},
  {'k', {{}, ']'}},
  {'l', {{'shift'}, ']'}},
  {';', {{'shift'}, '.'}},
  {"'", {{}, '`'}},
  {'r', {{}, '\\'}},
  {'u', {{}, '/'}},
  {'v', {{'shift'}, ','}},
  {'m', {{'shift'}, '.'}},
  {'n', {{'shift'}, '\\'}},
  {'t', {{'shift'}, '='}},
  {'y', {{}, '-'}},
}

for i, keyTable in ipairs(symbolsTable) do
  k:bind(
    {},
    keyTable[1],
    function()
      hs.eventtap.keyStroke(unpack(keyTable[2]))
      k.triggered = true
    end
  )
end

hyperTable = {'q'}

for i,key in ipairs(hyperTable) do
  k:bind(
    {},
    key,
    nil,
    function()
      hs.eventtap.keyStroke({'cmd','shift','ctrl'}, key)
      k.triggered = true
    end
  )
end

hyperAppsTable = { {'w', 'Emacs'} }

for i, app in ipairs(hyperAppsTable) do
  k:bind(
    {},
    app[1],
    nil,
    function()
      launch(app[2])
      k:exit()
    end
  )
end

-- Sequential keybindings
appModal = hs.hotkey.modal.new({}, "F16")

enterAppMode = function()
  k.triggered = true
  appModal:enter()
end

releasedEnter = function()
end

k:bind({}, 'return', enterAppMode, releasedEnter)

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
