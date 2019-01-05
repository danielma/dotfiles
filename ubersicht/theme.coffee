refreshFrequency: false

command: "cat ~/.base16_theme"

termColorsToBaseMap:
  "00": "00"
  "01": "08"
  "02": "0B"
  "03": "0A"
  "04": "0D"
  "05": "0E"
  "06": "0C"
  "07": "05"
  "08": "03"
  "15": "07"
  "16": "09"
  "17": "0F"
  "18": "01"
  "19": "02"
  "20": "04"
  "21": "06"

parseThemeInfo: (themeInfo) ->
  termColorToBase = (termColorKey) ->
    
  colorToHex = (termColor) ->
    if termColor.match(/^\$/)
      termColor
    else   
      "##{termColor.replace(/\//g, "")}"

  colors = {}
  termColorsToBaseMap = @termColorsToBaseMap

  themeInfo.match(/^color\d\d="[\d\w\/]+/gm).map (pair) ->
    [base, color] = pair.match(/color(\d\d)="?([\d\w\/]+)/).slice(1, 3)

    base = "base#{termColorsToBaseMap[base]}"

    unless color.match(/^\$/)
      colors[base] = colorToHex(color)

  colors

flattenArray: (array) ->
  [].concat.apply([], array)

buildCss: (colors) ->
  backgrounds = Object.entries(colors).map ([key, value]) ->
    ["FF", "A5", "D0"].map (opacity) ->
      ".bg-#{key}-#{opacity} { background-color: #{value}#{opacity}; } " +
      ".bg-#{key}-#{opacity}-important { background-color: #{value}#{opacity} !important; }"
  foregrounds = Object.entries(colors).map ([key, value]) ->
    ".fg-#{key} { color: #{value}; } " +
    ".fg-#{key}-important { color: #{value} !important; }"
  @flattenArray(backgrounds.concat(foregrounds)).join(" ")

render: (themeInfo) ->
  themeInfo = @parseThemeInfo(themeInfo)
  css = @buildCss(themeInfo)
  """
    <style>#{css}</style>
  """
