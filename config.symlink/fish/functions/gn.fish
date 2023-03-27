function __core_notifications --description 'Github notifications'
    ghub notifications 2>/dev/null | jq -r 'group_by(.repository.full_name)
    | .[]
    | .[]
    | . * { html_url: .subject.url | sub("api\\\."; "") | sub("github.com/repos"; "github.com") | sub("/pulls/"; "/pull/") }
    | @text "\u001B[1;32m\(.subject.title)\u001B[0m\t\u001B[0;33m\(.repository.full_name)\u001B[0m\t\(.html_url)\t\(.url)"' |
        tail -r
end

function gn
    argparse p/plain -- $argv

    if set -q _flag_plain
        __core_notifications
    else
        __core_notifications | fzf \
            --ansi \
            --delimiter "\t" --with-nth=1..2 --tabstop=1 \
            --bind 'ctrl-o:execute-silent(open -g {3})' \
            --bind 'ctrl-t:execute-silent(ghub -X PATCH {4})+reload-sync(fish -c "gn --plain")' \
            --bind 'enter:become(open {3})'
    end
end
