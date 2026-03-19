function gws
    set worktree (git worktree list | _fzf_wrapper | awk '{print $1}')
    and cd $worktree
end
