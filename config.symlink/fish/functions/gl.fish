function gl --description 'Git pull'
    # set --local options 'h/help' 'g/graph'

    # argparse $options -- $argv

    # if set --query _flag_help
    #     printf "Usage: foo [OPTIONS]\n\n"
    #     printf "Options:\n"
    #     printf "  -h/--help    Prints help and exits\n"
    #     printf "  -g/--graph   Displays as graph"
    #     return 0
    # end

    # if set --query _flag_graph
    #     git log --oneline --graph
    # else
    #     _fzf_search_git_log
    # end

    git pull --prune
end
