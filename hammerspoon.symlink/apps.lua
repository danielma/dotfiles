local hyper = {"ctrl", "alt", "cmd", "shift"}

local appHotkeys = {
  C = "Arc",
  G = "cmux",
  S = "Slack",
}

for key, appName in pairs(appHotkeys) do
  hs.hotkey.bind(hyper, key, function()
    hs.application.launchOrFocus(appName)
  end)
end
