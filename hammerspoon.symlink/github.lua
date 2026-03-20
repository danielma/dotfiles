local hyper = {"ctrl", "alt", "cmd", "shift"}

local repos = {
  "danielma/dotfiles",
}

local choices = {}

for _, repo in ipairs(repos) do
  table.insert(choices, {
    text = repo,
    subText = "Open on GitHub",
    repo = repo,
  })
end

local chooser = hs.chooser.new(function(choice)
  if not choice then
    return
  end

  hs.urlevent.openURL("https://github.com/" .. choice.repo)
end)

chooser:choices(choices)
chooser:searchSubText(true)
chooser:placeholderText("Jump to GitHub repo")

hs.hotkey.bind(hyper, "G", function()
  chooser:show()
end)
