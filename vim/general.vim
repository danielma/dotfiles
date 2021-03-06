if !exists("g:reloaded")
  let g:reloaded = 0
endif

let mapleader="\<Space>"
syntax enable
filetype plugin indent on

set relativenumber
set number
set foldcolumn=4
set foldlevel=2
set foldmethod=indent
if !g:reloaded
  set encoding=utf-8
endif

set colorcolumn=100

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

set exrc " Allow for custom config

let g:user_emmet_settings = { 'indentation': '  ' }
let g:nvim_true_color = $NVIM_TUI_ENABLE_TRUE_COLOR

let g:indent_guides_force_gui_colors = 1

if !exists(":Reload")
  function! s:ReloadVimRC()
    let g:reloaded = 1
    source $MYVIMRC
  endfunction

  command Reload call s:ReloadVimRC()
endif

vmap <Enter> <Plug>(EasyAlign)
nmap <Leader>a <Plug>(EasyAlign)

nmap <Leader>b :BuffergatorToggle<CR>
nmap <Leader>bd :Kwbd<CR>

nmap <Leader>ff :set foldmethod=syntax<CR>:set foldmethod=manual<CR>
:command! -bar -bang Q quit<bang>
:command! -bar -bang W write<bang>

" call add(g:extra_whitespace_ignored_filetypes, 'esv')
" autocmd Filetype esv call SetupESV()
"
" function! SetupESV()
"   setl listchars=""
"   setl laststatus=1
" endfunction

"let g:pencil_neutral_headings = 1
"let g:pencil_terminal_italics = 1
"let g:pencil#wrapModeDefault = 'soft'
"augroup pencil
"  autocmd!
"  autocmd FileType markdown,mkd call pencil#init()
"  autocmd FileType markdown,mkd :Goyo 80
"  autocmd FileType text         call pencil#init()
"augroup END
"

let g:auto_save = 0

function! g:NoteEdit()
  let g:deoplete#enable_at_startup = 0
  let b:auto_save = 1
  let b:goyo_width = "100%"
  let b:goyo_height = "100%"
  nmap q :qa<CR>
  execute "Goyo"
endfunction

command NoteEdit call g:NoteEdit()
