local hyper = {"ctrl", "alt", "cmd"}

local modules = {
  "reload",
  "window",
  "apps",
}

for _, module in ipairs(modules) do
  local ok, err = pcall(require, module)
  if not ok then
    hs.alert.show("Failed loading " .. module .. ": " .. tostring(err))
  end
end

hs.alert.show("Hammerspoon config loaded")
