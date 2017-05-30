function git_dirty
        if (git status -s &> /dev/null)
                echo ""
        else
                echo (git symbolic-ref HEAD 2>/dev/null)
        end
end

function fish_prompt --description 'Write out the prompt'
        set -l color_cwd
    set -l suffix
    switch $USER
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
            end
            set suffix '#'
        case '*'
            set color_cwd $fish_color_cwd
            set suffix '>'
    end

    echo
    echo -s -n (set_color $color_cwd) (prompt_pwd) (set_color normal)
    echo (git_dirty)
    echo -s \u276F ' '
end

function c
        cd ~/Code/$argv
end
