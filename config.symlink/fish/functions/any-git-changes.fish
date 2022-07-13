function any-git-changes -d "t/f if there are any git changes"
    test (count (git status --porcelain)) -gt 0
end
