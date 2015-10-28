journal() {
  date +"%Y-%m-%d %H:%M%n%n" | vim -c "Simplenote -n" -
}

alias nva="/Applications/Neovim.app/Contents/MacOS/Neovim . &"
