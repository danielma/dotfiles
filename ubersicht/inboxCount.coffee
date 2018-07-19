command: "tmux list-windows -t inbox -F \"#W\" 2>&1 | grep inbox-cli | sed \"s/^inbox-cli (\\([0-9]*\\))/\\1/\" || true"

refreshFrequency: '1m'

# render gets called after the shell command has executed. The command's output
# is passed in as a string. Whatever it returns will get rendered as HTML.
render: (output) -> parseInt(output, 10)

update: (output, domEl) ->
  el = $("#__uebersicht").find(".inboxCount")

  if output > 0
    el
      .html(output)
      .removeClass("bg-base0B-FF-important")
  else
    el
      .html("✔")
      .addClass("bg-base0B-FF-important")

style: "display: none"
