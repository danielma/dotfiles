command: 'date +"%-m/%-e | %-l:%M"'

refreshFrequency: '1m'

# render gets called after the shell command has executed. The command's output
# is passed in as a string. Whatever it returns will get rendered as HTML.
render: (output) -> output

update: (output, domEl) ->
  $("#uebersicht").find(".time").html(output)

style: "display: none"
