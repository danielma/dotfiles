local hyper = {"ctrl", "alt", "cmd", "shift"}

local appHotkeys = {
   ["1"] = "1Password",
   A = "Asana",
   C = "Arc",
   D = "Dash",
   E = "Emacs",
   G = "cmux",
   I = "Music",
   L = "Notion Calendar",
   M = "Messages",
   N = "Notion",
   S = "Slack",
   W = "Obsidian",
}

for key, appName in pairs(appHotkeys) do
  hs.hotkey.bind(hyper, key, function()
    local frontmostApp = hs.application.frontmostApplication()

    if frontmostApp and frontmostApp:name() == appName then
       frontmostApp:hide()
    else
       hs.application.launchOrFocus(appName)
    end
  end)
end
