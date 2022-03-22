function gbs
    git branch --sort=-committerdate | grep -v '^*' | _fzf_wrapper | xargs git checkout
end
