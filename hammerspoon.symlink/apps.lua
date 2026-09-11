local hyper = {"ctrl", "alt", "cmd", "shift"}

local appHotkeys = {
   ["1"] = "com.1password.1password",
   C = "company.thebrowser.Browser",
   D = "com.kapeli.dash-setapp",
   E = "org.gnu.Emacs",
   G = "app.supabit.supacode",
   H = "com.culturedcode.ThingsMac",
   I = "com.apple.Music",
   L = "com.cron.electron",
   M = "com.apple.MobileSMS",
   N = "notion.id",
   S = "com.tinyspeck.slackmacgap",
   T = "com.linear",
   W = "md.obsidian",
}

for key, bundleID in pairs(appHotkeys) do
  hs.hotkey.bind(hyper, key, function()
    local frontmostApp = hs.application.frontmostApplication()

    if frontmostApp and frontmostApp:bundleID() == bundleID then
       frontmostApp:hide()
    else
       hs.application.launchOrFocusByBundleID(bundleID)
    end
  end)
end
