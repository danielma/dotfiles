command: "tmux list-windows -t inbox -F \"#W\" 2>&1 | grep inbox-cli | sed \"s/^inbox-cli (\\([0-9]*\\))/\\1/\" || true"

refreshFrequency: '1m'

# render gets called after the shell command has executed. The command's output
# is passed in as a string. Whatever it returns will get rendered as HTML.
render: (output) -> parseInt(output, 10)

update: (output, domEl) ->
  if output > 0
    $("#__uebersicht").find(".inboxCount")
      .html(output)
      .removeClass("bg-base0B-FF-important fg-base00-important")
  else
    $("#__uebersicht").find(".inboxCount")
      .html("✔")
      .addClass("bg-base0B-FF-important fg-base00-important")

style: "display: none"
