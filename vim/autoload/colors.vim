
set guifont=Menlo:h11
set cursorline
" execute "set bg=".base16_shell_theme[1]
" execute "colorscheme ".base16_shell_theme[0]
set bg=dark

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

"colorscheme base16-ocean
"colorscheme onedark

" Customizations
hi Search guibg=NONE guifg=NONE gui=underline
