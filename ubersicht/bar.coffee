refreshFrequency: false

render: () -> """
  <div class="inboxCount"></div>
  <div class="currentWindow"></div>
  <div class="bigSpace"></div>
  <div class="musicBar"></div>
  <div class="time"></div>
"""

# the CSS style for this widget, written using Stylus
# (http://learnboost.github.io/stylus/)
style: """
  left: 10px
  right: 10px
  bottom: 10px
  background: rgba(#111, 0.65)
  -webkit-backdrop-filter: blur(20px)
  border-radius: 5px
  box-sizing: border-box
  color: #FAFAFA
  font-family: IBM Plex Mono
  font-weight: 400
  line-height: 1
  -webkit-font-smoothing: antialiased
  display: flex

  > *
    padding: 10px

  > :last-child
    border-top-right-radius: 5px
    border-bottom-right-radius: 5px
        
  .bigSpace
    flex: 2

  .inboxCount
    background-color: #82A2BD
    // color: #122418

  .musicBar
    background-color: #DC9364
    color: #122418

  .time
    background-color: #EFC57A
    color: #122418
"""
