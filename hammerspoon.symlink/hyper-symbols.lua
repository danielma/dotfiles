-- A global variable for the Hyper Mode
hyperModal = hs.hotkey.modal.new({}, 'F17')

-- Enter Hyper Mode when F18 (Hyper) is pressed
pressedF18 = function()
  hyperModal.triggered = false
  hyperModal.symbolsOnly = false
  hyperModal:enter()
end

-- Leave Hyper Mode when F18 (Hyper) is pressed,
--   send ESCAPE if no other keys are pressed.
releasedF18 = function()
  hyperModal:exit()
  if not hyperModal.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Bind the Hyper key
f18 = hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)

-- Symbols Layer
-- .-----------------------------------------------------------------------.
-- |     |     |     |     |     |     |     |     |     |     |     |     |
-- |-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----|
-- |     |  q  |  w  |  e  |  \  |  +  |  -  |  u  |  i  |  o  |  p  |     |
-- |-----+-----+-----+-----+-----+-----------+-----+-----+-----+-----+-----|
-- |     | ^-s |  {  |  [  |  (  |  =  |  _  |  )  |  ]  |  }  |  >  |  `  |
-- |-----+-----+-----+-----+-----+-----|-----+-----+-----+-----+-----+-----|
-- |     |  z  |  x  |  %  |  <  |  &  |  |  |  >  |  ,  |  .  |  /  |     |
-- `-----------------------------------------------------------------------'

symbolsTable = {
  {'a', {{'ctrl'}, 's'}},
  {'b', {{'shift'}, '7'}},
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
  -- {'u', {{}, '/'}},
  {'v', {{'shift'}, ','}},
  {'m', {{'shift'}, '.'}},
  {'n', {{'shift'}, '\\'}},
  {'t', {{'shift'}, '='}},
  {'y', {{}, '-'}},
}

for i, keyTable in ipairs(symbolsTable) do
  hyperModal:bind(
    {},
    keyTable[1],
    function()
       hyperModal.triggered = true
       hs.eventtap.keyStroke(unpack(keyTable[2]))
    end
  )
end

hyperTable = {'q'}

for i,key in ipairs(hyperTable) do
  hyperModal:bind(
    {},
    key,
    function()
       hyperModal.triggered = true
       hs.eventtap.keyStroke({'cmd','shift','ctrl'}, key)
    end
  )
end

hyperAppsTable = { {'w', 'Emacs'} }

for i, app in ipairs(hyperAppsTable) do
  hyperModal:bind(
    {},
    app[1],
    nil,
    function()
      launch(app[2])
      hyperModal.triggered()
      hyperModal:exit()
    end
  )
end
