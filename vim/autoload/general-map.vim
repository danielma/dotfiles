" Splits
map <C-J> <C-W>j
map <Leader>j <C-W>j
map <C-K> <C-W>k
map <Leader>k <C-W>k
map <C-h> <C-W>h
map <Leader>h <C-W>h
map <C-l> <C-W>l
map <Leader>l <C-W>l
map <Leader>= <C-W>=

" Saving
map <C-s> :w<CR>
map <Leader>w :w<CR>
imap <C-S> <C-O><C-S>
noremap <D-s> :w<CR>

" Close file
map <C-Q> :q<CR>
map <Leader>q :q<CR>
imap <C-Q> <C-O><C-Q>

map <Leader>/ :call NERDComment('n', 'Toggle')<CR>

" Mkdir
nmap <silent> <leader>md :!mkdir -p %:p:h<CR>

" Insert mode
imap <C-d> <Esc>ddi

