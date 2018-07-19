refreshFrequency: false

render: (themeInfo) ->
  """
    <div id="bar" class="fg-base05 bg-base00-A5">
      <div class="inboxCount bg-base09-FF fg-base02"></div>
      <div class="currentWindow"></div>
      <div class="bigSpace"></div>
      <div class="musicBar bg-base0D-FF fg-base00"></div>
      <div class="time bg-base05-FF fg-base00"></div>
    </div>
  """

style: """
  left: 10px
  right: 10px
  bottom: 10px

  #bar
    -webkit-backdrop-filter: blur(20px)
    border-radius: 5px
    box-sizing: border-box
    font-family: IBM Plex Mono
    font-size: 14px
    font-weight: normal
    line-height: 1
    -webkit-font-smoothing: antialiased
    display: flex

    > *
      padding: 10px

      &:empty
        padding: 0

    > :first-child
      border-top-left-radius: 5px
      border-bottom-left-radius: 5px

    > :last-child
      border-top-right-radius: 5px
      border-bottom-right-radius: 5px

    .bigSpace
      flex: 2
"""
