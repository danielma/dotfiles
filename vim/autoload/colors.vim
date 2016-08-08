let g:base16_shell_path="~/.dotfiles/base16-shell/"
" let base16_shell_theme=split($BASE16_THEME, "\\.")
let base16_shell_theme=$BASE16_THEME

set guifont=Menlo:h11
set cursorline
" execute "set bg=".base16_shell_theme[1]
" execute "colorscheme ".base16_shell_theme[0]
set bg=dark
execute "colorscheme ".base16_shell_theme
"colorscheme base16-ocean
"colorscheme onedark

" Customizations
hi Search guibg=NONE guifg=NONE gui=underline
