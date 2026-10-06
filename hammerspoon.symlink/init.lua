local hyper = {"ctrl", "alt", "cmd"}

local herdrConfig = os.getenv("HOME") .. "/.dotfiles/config.symlink/herdr/config.toml"
hs.execute(string.format("/bin/launchctl setenv HERDR_CONFIG_PATH %q", herdrConfig))

local modules = {
  "reload",
  "window",
  "apps",
  "browse"
}

for _, module in ipairs(modules) do
  local ok, err = pcall(require, module)
  if not ok then
    hs.alert.show("Failed loading " .. module .. ": " .. tostring(err))
  end
end

hs.alert.show("Hammerspoon config loaded")
