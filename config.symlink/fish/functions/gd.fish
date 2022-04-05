function gd --description 'Git Diff' --wraps "git diff"
    git diff $argv; 
end
