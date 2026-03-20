local hyper = {"ctrl", "alt", "cmd", "shift"}
local cacheTtlSeconds = 300

local choices = {}
local lastLoadedAt = 0
local chooser

local function repoChoicesFromJson(stdout)
  local repos = hs.json.decode(stdout)

  if type(repos) ~= "table" then
    return nil, "Could not parse GitHub repo list"
  end

  local nextChoices = {}

  for _, repo in ipairs(repos) do
    table.insert(nextChoices, {
      text = repo.nameWithOwner,
      subText = repo.description or repo.url,
      url = repo.url,
    })
  end

  table.sort(nextChoices, function(left, right)
    return left.text < right.text
  end)

  return nextChoices
end

local function setLoadingChoices()
  chooser:choices({
    {
      text = "Loading repos...",
      subText = "Fetching from gh",
    },
  })
end

local function loadRepos(onComplete)
  local now = os.time()

  if #choices > 0 and (now - lastLoadedAt) < cacheTtlSeconds then
    onComplete(true)
    return
  end

  local task = hs.task.new("/bin/zsh", function(exitCode, stdout, stderr)
    if exitCode ~= 0 then
      hs.alert.show("gh repo list failed")
      if stderr and stderr ~= "" then
        print(stderr)
      end
      onComplete(false)
      return
    end

    local nextChoices, err = repoChoicesFromJson(stdout)

    if not nextChoices then
      hs.alert.show(err)
      onComplete(false)
      return
    end

    choices = nextChoices
    lastLoadedAt = os.time()
    chooser:choices(choices)
    onComplete(true)
  end, {"-lc", "gh repo list " .. githubOrg .. " --limit 200 --json nameWithOwner,url,description"})

  if not task then
    hs.alert.show("Could not start gh")
    onComplete(false)
    return
  end

  task:start()
end

chooser = hs.chooser.new(function(choice)
  if not choice then
    return
  end

  if choice.url then
    hs.urlevent.openURL(choice.url)
  end
end)

chooser:choices(choices)
chooser:searchSubText(true)
chooser:placeholderText("Jump to GitHub repo")

hs.hotkey.bind(hyper, "G", function()
  setLoadingChoices()
  chooser:show()
  loadRepos(function(success)
    if success then
      chooser:choices(choices)
    end
  end)
end)
