
" set guifont=Menlo:h11
set cursorline
" execute "set bg=".base16_shell_theme[1]
" execute "colorscheme ".base16_shell_theme[0]
" set bg=dark

if filereadable(expand("~/.base16_theme-name"))
  let base16colorspace=256
  set bg=dark
  execute "colorscheme base16-".join(readfile(expand("~/.base16_theme-name")), "")
else
  colorscheme base16-default-light
endif

"colorscheme base16-ocean
"colorscheme onedark

" Customizations
hi Search guibg=NONE guifg=NONE gui=underline
