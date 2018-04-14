command: "tmux list-windows -t inbox -F \"#W\" 2>&1 | grep inbox-cli | sed \"s/^inbox-cli (\\([0-9]*\\))/\\1/\" || true"

refreshFrequency: '1m'

# render gets called after the shell command has executed. The command's output
# is passed in as a string. Whatever it returns will get rendered as HTML.
render: (output) -> output

update: (output, domEl) ->
  if output
    $("#__uebersicht").find(".inboxCount").html(output)
  else
    $("#__uebersicht").find(".inboxCount").html("")

style: "display: none"
