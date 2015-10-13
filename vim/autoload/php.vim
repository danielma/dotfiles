let g:php_mode = 'php'
function! TogglePHPMode()
  if g:php_mode == 'php'
    let g:php_mode = 'html'
    set filetype=html
    set syntax=php
  else
    let g:php_mode = 'php'
    set filetype=php
  endif
endfunction
command! TogglePHPMode call TogglePHPMode()
nmap <Leader>p :TogglePHPMode<CR>
