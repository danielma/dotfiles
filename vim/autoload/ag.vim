set grepprg=ag\ --nogroup\ --nocolor

nnoremap <Leader>ak :grep! "\b<C-R><C-W>\b"<CR>:cw<CR><CR>
map <Leader>pa :Ag<CR>
