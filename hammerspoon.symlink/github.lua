local hyper = {"ctrl", "alt", "cmd", "shift"}
local githubScriptPath = os.getenv("HOME") .. "/.dotfiles/bin/github-repo-choices"
local log = hs.logger.new("github", "info")

local choices = {}
local chooser
local isLoading = false

local function trim(value)
  return (value:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function setStatusChoice(text, subText)
  chooser:choices({
    {
      text = text,
      subText = subText,
    },
  })
end

local function applyStatusLine(line)
  local status = line:match("^STATUS:%s*(.+)$")

  if status then
    setStatusChoice("Loading repos...", status)
  elseif line ~= "" then
    log.i(line)
  end
end

local function runTask(command, args, onComplete)
  local stdoutParts = {}
  local stderrParts = {}
  local stderrBuffer = ""

  local task = hs.task.new(command, function(exitCode)
    if stderrBuffer ~= "" then
      applyStatusLine(trim(stderrBuffer))
    end

    onComplete(exitCode, table.concat(stdoutParts), table.concat(stderrParts))
  end, function(_, stdout, stderr)
    if stdout and stdout ~= "" then
      table.insert(stdoutParts, stdout)
    end

    if stderr and stderr ~= "" then
      table.insert(stderrParts, stderr)
      stderrBuffer = stderrBuffer .. stderr

      while true do
        local newlineStart, newlineEnd = stderrBuffer:find("\n")

        if not newlineStart then
          break
        end

        local line = stderrBuffer:sub(1, newlineStart - 1)
        stderrBuffer = stderrBuffer:sub(newlineEnd + 1)
        applyStatusLine(trim(line))
      end
    end

    return true
  end, args or {})

  if not task then
    return nil
  end

  task:start()
  return task
end

local function choicesFromJson(stdout)
  local decoded = hs.json.decode(stdout)

  if type(decoded) ~= "table" then
    return nil, "Could not parse repo chooser JSON"
  end

  if #decoded == 0 then
    return nil, "No GitHub repos found"
  end

  return decoded
end

local function loadRepos(onComplete)
  if isLoading then
    setStatusChoice("Loading repos...", "Already fetching GitHub repos")
    onComplete(false)
    return
  end

  isLoading = true

  local task = runTask(githubScriptPath, {}, function(exitCode, stdout, stderr)
    isLoading = false

    if exitCode ~= 0 then
      local message = trim(stderr)
      hs.alert.show("GitHub repo chooser failed")
      setStatusChoice("Repo loading failed", "See Hammerspoon console for details")
      log.e("github-repo-choices failed: " .. message)
      onComplete(false)
      return
    end

    local nextChoices, err = choicesFromJson(stdout)

    if not nextChoices then
      hs.alert.show(err)
      setStatusChoice("Repo loading failed", err)
      log.e(err)
      onComplete(false)
      return
    end

    choices = nextChoices
    chooser:choices(choices)
    log.i(string.format("Loaded %d repo choices", #choices))
    onComplete(true)
  end)

  if not task then
    isLoading = false
    hs.alert.show("Could not start repo chooser script")
    setStatusChoice("Repo loading failed", githubScriptPath)
    log.e("Could not start repo chooser script: " .. githubScriptPath)
    onComplete(false)
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
  setStatusChoice("Loading repos...", "Running repo chooser script")
  chooser:show()
  loadRepos(function(success)
    if success then
      chooser:choices(choices)
    end
  end)
end)
