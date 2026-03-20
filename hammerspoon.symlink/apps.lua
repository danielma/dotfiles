local hyper = {"ctrl", "alt", "cmd", "shift"}

local appHotkeys = {
   ["1"] = "1Password",
   C = "Arc",
   E = "Emacs",
   G = "cmux",
   M = "Messages",
   S = "Slack",
   W = "Obsidian",
}

for key, appName in pairs(appHotkeys) do
  hs.hotkey.bind(hyper, key, function()
    hs.application.launchOrFocus(appName)
  end)
end
