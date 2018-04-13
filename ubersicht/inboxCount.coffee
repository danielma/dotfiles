command: "tmux list-windows -t inbox -F \"#W\" | grep inbox-cli | sed \"s/^inbox-cli (\\([0-9]*\\))/\\1/\""

refreshFrequency: '1m'

# render gets called after the shell command has executed. The command's output
# is passed in as a string. Whatever it returns will get rendered as HTML.
render: (output) -> output

update: (output, domEl) ->
  $("#__uebersicht").find(".inboxCount").html(output)

style: "display: none"
