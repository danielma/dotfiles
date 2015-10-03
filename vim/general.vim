let mapleader=","
syntax enable

set relativenumber
set foldcolumn=4
set foldlevel=2
set foldmethod=manual
set encoding=utf-8

set colorcolumn=80

set noswapfile
set nobackup
set nowb

set expandtab
set smarttab
set shiftwidth=2
set tabstop=2

set ai
set si
set wrap

let g:user_emmet_settings = { 'indentation': '  ' }

vmap <Enter> <Plug>(EasyAlign)
nmap <Leader>a <Plug>(EasyAlign)

nmap <Leader>b :BuffergatorToggle<CR>
nmap <Leader>bd :Kwbd<CR>

nmap <Leader>f :set foldmethod=syntax<CR>:set foldmethod=manual<CR>
:command! -bar -bang Q quit<bang>
:command! -bar -bang W write<bang>

map <D-s> :w

autocmd BufNewFile,BufReadPost *.haml setl foldmethod=indent
autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent
autocmd BufRead,BufNewFile *.es6 setfiletype javascript

" call add(g:extra_whitespace_ignored_filetypes, 'esv')
" autocmd Filetype esv call SetupESV()
" 
" function! SetupESV()
"   setl listchars=""
"   setl laststatus=1
" endfunction

nnoremap <silent> <Leader>rr :TagbarOpenAutoClose<CR>

if filereadable('~/.simplenoterc')
  source ~/.simplenoterc
endif
let g:SimplenoteFiletype= "markdown"

let g:pencil_neutral_headings = 1
let g:pencil_terminal_italics = 1
let g:pencil#wrapModeDefault = 'soft'
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
  autocmd FileType markdown,mkd :Goyo 80
  autocmd FileType text         call pencil#init()
augroup END

function SetMarkdownOptions()
  " Enable spellcheck.
  set spell spelllang=en_us
  set spellcapcheck=
  set guifont=Cousine:h18
  colors pencil
  set background=light
  set go -=r
endfunction

function! s:goyo_enter()
  call SetMarkdownOptions()
  let b:quitting = 0
  let b:quitting_bang = 0
  autocmd QuitPre <buffer> let b:quitting = 1
  cabbrev <buffer> q! let b:quitting_bang = 1 <bar> q!
endfunction

function! s:goyo_leave()
  call SetDefaultVisualOptions()
  " Quit Vim if this is the only remaining buffer
  if b:quitting
    if b:quitting_bang
      q!
    else
      q
    endif
  endif
endfunction

autocmd! User GoyoEnter
autocmd! User GoyoLeave
autocmd  User GoyoEnter nested call <SID>goyo_enter()
autocmd  User GoyoLeave nested call <SID>goyo_leave()

let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exec = '/Users/danielma/Work/danielma/better/frontend/node_modules/.bin/eslint'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:jsx_ext_required = 0
