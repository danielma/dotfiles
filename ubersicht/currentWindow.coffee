command: '/usr/local/bin/chunkc tiling::query -d mode'

refreshFrequency: '2s'

# render gets called after the shell command has executed. The command's output
# is passed in as a string. Whatever it returns will get rendered as HTML.
render: (output) -> output

update: (output, domEl) ->
  $("#uebersicht").find(".currentWindow").html(output)

style: "display: none"
