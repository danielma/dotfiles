if exists('*MacMenu')
  call MacMenu("Window.Close Tab", "T-*")
  nnoremap <T-w> :q<CR>

  call MacMenu("File.Print", "T-#")
  nmap <T-p> <C-p>

  nmap <T-l> <C-l>
  nmap <T-k> <C-k>
  nmap <T-j> <C-j>
  nmap <T-h> <C-h>

  imap <T-Enter> <Esc>o
endif

