command: "cat ~/Music/.musicBar"

refreshFrequency: '2s'

# render gets called after the shell command has executed. The command's output
# is passed in as a string. Whatever it returns will get rendered as HTML.
render: (output) -> output

update: (output, domEl) ->
  $("#__uebersicht").find(".musicBar").html(output)

style: "display: none"
