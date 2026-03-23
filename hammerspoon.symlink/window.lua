local windPrefix = {"ctrl", "cmd"}

hs.window.animationDuration = 0

local function withFocusedWindow(fn)
  local win = hs.window.focusedWindow()
  if not win then
    hs.alert.show("No focused window")
    return
  end
  fn(win)
end

local function moveAndResize(unit)
  withFocusedWindow(function(win)
    win:move(unit, nil, true)
  end)
end

local function approxEqual(a, b)
  return math.abs(a - b) < 0.02
end

local function cycleHorizontal(direction)
  withFocusedWindow(function(win)
    local frame = win:frame()
    local screenFrame = win:screen():frame()
    local relativeX = (frame.x - screenFrame.x) / screenFrame.w
    local relativeW = frame.w / screenFrame.w

    local sizes = {0.5, 2 / 3, 1 / 3}
    local currentIndex = 0

    for index, size in ipairs(sizes) do
      local expectedX = direction == "left" and 0 or (1 - size)
      if approxEqual(relativeX, expectedX) and approxEqual(relativeW, size) then
        currentIndex = index
        break
      end
    end

    local nextSize = sizes[(currentIndex % #sizes) + 1]
    local nextX = direction == "left" and 0 or (1 - nextSize)

    win:move({x = nextX, y = 0.0, w = nextSize, h = 1.0}, nil, true)
  end)
end

hs.hotkey.bind(windPrefix, "up", function()
  withFocusedWindow(function(win)
    win:maximize(0)
  end)
end)

hs.hotkey.bind(windPrefix, "left", function()
  cycleHorizontal("left")
end)

hs.hotkey.bind(windPrefix, "right", function()
  cycleHorizontal("right")
end)

hs.hotkey.bind(windPrefix, "down", function()
  moveAndResize({x = 1 / 6, y = 1 / 6, w = 2 / 3, h = 2 / 3})
end)
