# Quick shortcut to an editor.
#
# This means that as I travel back and forth between editors, hey, I don't have
# to re-learn any arcane commands. Neat.
#
# USAGE:
#
#   $ e
#   # => opens the current directory in your editor
#
#   $ e .
#   $ e /usr/local
#   # => opens the specified directory in your editor

function e --description 'Edit a file or the current directory'
    if string match -qe emacsclient "$EDITOR"
        set editor_cmd "$EDITOR -n"
    else
        set editor_cmd "$EDITOR"
    end

    if set -q argv[1]
        eval "$editor_cmd $argv[1]"
    else
        eval "$editor_cmd ."
    end
end
