local hyper = {"ctrl", "alt", "cmd", "shift"}
local cacheTtlSeconds = 300
local log = hs.logger.new("github", "info")

local choices = {}
local lastLoadedAt = 0
local chooser
local isLoading = false

local function setStatusChoice(text, subText)
  chooser:choices({
    {
      text = text,
      subText = subText,
    },
  })
end

local function runTask(args, onComplete)
  local stdoutParts = {}
  local stderrParts = {}

  local task = hs.task.new("/bin/zsh", function(exitCode)
    onComplete(exitCode, table.concat(stdoutParts), table.concat(stderrParts))
  end, function(_, stdout, stderr)
    if stdout and stdout ~= "" then
      table.insert(stdoutParts, stdout)
    end

    if stderr and stderr ~= "" then
      table.insert(stderrParts, stderr)
      log.i(stderr)
    end

    return true
  end, args)

  if not task then
    return nil
  end

  task:start()
  return task
end

local function trim(value)
  return (value:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function splitLines(value)
  local lines = {}

  for line in value:gmatch("[^\r\n]+") do
    table.insert(lines, trim(line))
  end

  return lines
end

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
      pushedAt = repo.pushedAt or "",
    })
  end

  local function compareChoices(left, right)
    if left.pushedAt ~= right.pushedAt then
      return left.pushedAt > right.pushedAt
    end

    return left.text < right.text
  end

  if #nextChoices == 0 then
    return nil, "No GitHub repos found"
  end

  table.sort(nextChoices, compareChoices)

  return nextChoices
end

local function ownerListFromOutput(stdout)
  local owners = {}

  for _, owner in ipairs(splitLines(stdout)) do
    if owner ~= "" then
      table.insert(owners, owner)
    end
  end

  if #owners == 0 then
    return nil, "No GitHub owners found"
  end

  return owners
end

local function mergeRepos(existing, incoming)
  for _, repo in ipairs(incoming) do
    existing[repo.text] = repo
  end
end

local function fetchReposForOwners(owners, index, reposByName, onComplete)
  if index > #owners then
    local mergedRepos = {}

    for _, repo in pairs(reposByName) do
      table.insert(mergedRepos, repo)
    end

    table.sort(mergedRepos, function(left, right)
      if left.pushedAt ~= right.pushedAt then
        return left.pushedAt > right.pushedAt
      end

      return left.text < right.text
    end)

    onComplete(true, mergedRepos)
    return
  end

  local owner = owners[index]
  setStatusChoice("Loading repos...", string.format("%d/%d %s", index, #owners, owner))

  local task = runTask({"-lc", "gh repo list \"" .. owner .. "\" --limit 1000 --json nameWithOwner,url,description,pushedAt"}, function(exitCode, stdout, stderr)
    if exitCode ~= 0 then
      hs.alert.show("gh repo list failed for " .. owner)
      log.e("gh repo list failed for " .. owner .. ": " .. trim(stderr))
      onComplete(false, nil)
      return
    end

    local nextChoices, err = repoChoicesFromJson(stdout)

    if not nextChoices then
      hs.alert.show(err)
      log.e("Could not parse repos for " .. owner .. ": " .. err)
      onComplete(false, nil)
      return
    end

    mergeRepos(reposByName, nextChoices)
    fetchReposForOwners(owners, index + 1, reposByName, onComplete)
  end)

  if not task then
    hs.alert.show("Could not start gh for " .. owner)
    log.e("Could not start gh for " .. owner)
    onComplete(false, nil)
  end
end

local function loadRepos(onComplete)
  local now = os.time()

  if isLoading then
    setStatusChoice("Loading repos...", "Already fetching GitHub repos")
    onComplete(false)
    return
  end

  if #choices > 0 and (now - lastLoadedAt) < cacheTtlSeconds then
    onComplete(true)
    return
  end

  isLoading = true
  setStatusChoice("Loading repos...", "Fetching GitHub owners")

  local task = runTask({"-lc", "{ git config get github.user; gh api user/orgs --paginate --jq '.[].login'; }"}, function(exitCode, stdout, stderr)
    if exitCode ~= 0 then
      isLoading = false
      hs.alert.show("gh owner lookup failed")
      log.e("gh owner lookup failed: " .. trim(stderr))
      onComplete(false)
      return
    end

    local owners, err = ownerListFromOutput(stdout)

    if not owners then
      isLoading = false
      hs.alert.show(err)
      log.e(err)
      onComplete(false)
      return
    end

    fetchReposForOwners(owners, 1, {}, function(success, mergedRepos)
      isLoading = false

      if not success then
        onComplete(false)
        return
      end

      choices = mergedRepos
      lastLoadedAt = os.time()
      chooser:choices(choices)
      log.i(string.format("Loaded %d repos from %d owners", #choices, #owners))
      onComplete(true)
    end)
  end)

  if not task then
    isLoading = false
    hs.alert.show("Could not start gh")
    log.e("Could not start gh owner lookup")
    onComplete(false)
    return
  end
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
  setStatusChoice("Loading repos...", "Preparing GitHub query")
  chooser:show()
  loadRepos(function(success)
    if success then
      chooser:choices(choices)
    else
      chooser:query("")
    end
  end)
end)
