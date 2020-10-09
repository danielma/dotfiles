fw = hs.window.focusedWindow
hs.window.animationDuration = 0

-- window management

-- define window movement/resize operation mappings
local arrowMap = {
  Up = { half = { 0, 0, 1,.5}, movement = { 0,-20}, complement = "Left", resize = "Shorter" },
  Down = { half = { 0,.5, 1,.5}, movement = { 0, 20}, complement = "Right", resize = "Taller" },
  Left = { half = { 0, 0,.5, 1}, movement = {-20, 0}, complement = "Down", resize = "Thinner" },
  Right = { half = {.5, 0,.5, 1}, movement = { 20, 0}, complement = "Up", resize = "Wider" },
}

local function mehBind(key, pressedFn, releasedFn, repeatFn)
   return hs.hotkey.bind(
      {"ctrl", "alt", "shift"}, 
      key,
      function()
         if pressedFn then
            pressedFn()
         end
      end,
      function()
         if releasedFn then
            releasedFn()
         end
      end,
      function()
         if repeatFn then
            repeatFn()
         end
      end
   )
end

local function hyperBind(key, pressedFn, releasedFn, repeatFn)
   return hyperModal:bind(
      {},
      key,
      function()
         if pressedFn then
            hyperModal.triggered = true
            pressedFn()
         end
      end,
      function()
         if releasedFn then
            hyperModal.triggered = true
            releasedFn()
         end
      end,
      function()
         if repeatFn then
            hyperModal.triggered = true
            repeatFn()
         end
      end
   )
end

-- compose screen quadrants from halves
local function quadrant(t1, t2)
  return {t1[1] + t2[1], t1[2] + t2[2], .5, .5}
end

-- move and/or resize windows
local function rect(rect)
  return function()
    undo:push()
    local win = fw()
    if win then win:move(rect) end
  end
end

-- center and enlarge current window; hold to maximize
hyperBind("space", rect({0, 0, 1, 1}))
mehBind("space", rect({0, 0, 1, 1}))

-- arrow-based window movement/resize operations
hs.fnutils.each({"Left", "Right", "Up", "Down"}, function(arrow)

    hyperBind(arrow, -- set to screen halves; hold for quadrants
      rect(arrowMap[arrow].half),
      nil,
      rect(quadrant(arrowMap[arrow].half, arrowMap[arrowMap[arrow].complement].half))
    )

    hs.hotkey.bind({"ctrl", "cmd"}, arrow, -- move windows incrementally
      rect(arrowMap[arrow].movement),
      nil,
      rect(arrowMap[arrow].movement)
    )

    -- hs.hotkey.bind({"ctrl", "alt"}, arrow, -- move windows by grid increments
    --   function() undo:push(); hs.grid['pushWindow'..arrow](fw()) end
    -- )

    mehBind(arrow, rect(arrowMap[arrow].half))
    -- hs.hotkey.bind({"ctrl", "alt", "shift"}, arrow, -- resize windows by grid increments
    --   -- function() undo:push(); hs.grid['resizeWindow'..arrowMap[arrow].resize](fw()) end
    --   rect(arrowMap[arrow].half)
    -- )
  end)

hyperBind("7", rect({ .0, 0, .5, .5 }))
hyperBind("9", rect({ .5, 0, .5, .5 }))
hyperBind("1", rect({ 0, .5, .5, .5 }))
hyperBind("3", rect({ .5, .5, .5, .5 }))
mehBind("7", rect({ .0, 0, .5, .5 }))
mehBind("9", rect({ .5, 0, .5, .5 }))
mehBind("1", rect({ 0, .5, .5, .5 }))
mehBind("3", rect({ .5, .5, .5, .5 }))

hs.hotkey.bind({"shift", "alt", "cmd"}, "Left", function() fw():moveOneScreenWest() end)
hs.hotkey.bind({"shift", "alt", "cmd"}, "Right", function() fw():moveOneScreenEast() end)

-- window grid configuration
hs.grid.setGrid("6x4")
hs.grid.setMargins({0, 0})
hyperBind('/', function()
    local gridSize = hs.grid.getGrid()
    hs.grid.setGrid("6x4")
    hs.grid.show(function() hs.grid.setGrid(gridSize) end)
  end)
mehBind('/', function()
    local gridSize = hs.grid.getGrid()
    hs.grid.setGrid("6x4")
    hs.grid.show(function() hs.grid.setGrid(gridSize) end)
  end)

-- undo for window operations
undo = {}

function undo:push()
  local win = fw()
  if win and not undo[win:id()] then
    self[win:id()] = win:frame()
  end
end

function undo:pop()
  local win = fw()
  if win and self[win:id()] then
    win:setFrame(self[win:id()])
    self[win:id()] = nil
  end
end

hs.hotkey.bind({"ctrl", "alt"}, "z", function() undo:pop() end)

--- isolate window

isolate = {}
isolateWf = hs.window.filter.new()
isolateWf:setCurrentSpace(true)
isolateWf:setSortOrder(hs.window.filter.sortByFocused)

function toggleIsolate()
   local windows = isolateWf:getWindows()
   local focusedWindow = fw()
   local moreThanOneVisibleWindow = windows[2]

   if moreThanOneVisibleWindow then
      isolate = {}
      for i, window in pairs(windows) do
         if window:id() ~= focusedWindow:id() then
            isolate[i] = window:id()
            window:application():hide()
         end
      end

      focusedWindow:focus()

      return
   end

   for i, id in pairs(isolate) do
      hadAnyStoredWindows = true

      hs.window.find(id):application():unhide()
   end
end

hs.hotkey.bind({"cmd", "alt"}, "h", function() toggleIsolate() end)
