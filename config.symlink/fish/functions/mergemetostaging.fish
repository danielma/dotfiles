function mergemetostaging
    if any-git-changes
        echo "Nope! You have changes"
        gs
        false
    else
        set current_branch (git symbolic-ref HEAD | sed 's/refs\/heads\///')
        git checkout staging && \
            git fetch origin staging && \
            git reset --hard origin/staging && \
            git merge $current_branch --no-edit && \
            git push
    end
end
