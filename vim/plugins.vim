call plug#begin('~/.vim/plugged')


" Tools
"Plug 'scrooloose/nerdtree'
"Plug 'scrooloose/nerdcommenter'
"Plug 'skalnik/vim-vroom'
"Plug 'tpope/vim-rbenv'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Plug 'benekastah/neomake'
"Plug 'easymotion/vim-easymotion'
"Plug 'junegunn/vim-easy-align'
Plug 'terryma/vim-multiple-cursors'
"Plug 'danielma/vim-indent-guides'
Plug 'bronson/vim-trailing-whitespace'
""Plug 'powerline/powerline'
"Plug 'jeetsukumaran/vim-buffergator'
"Plug 'rking/ag.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
"Plug 'daylerees/colour-schemes', { 'rtp': 'vim/' }
Plug 'rgarver/Kwbd.vim'
""Plug 'tpope/vim-fugitive'
"Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-commentary'
"Plug 'mattn/emmet-vim'

"" Colors
"Plug 'chriskempson/vim-tomorrow-theme'
"Plug 'altercation/vim-colors-solarized'
"Plug 'joshdick/onedark.vim'
Plug 'chriskempson/base16-vim'

"" Languages
"Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
"Plug 'pangloss/vim-javascript'
"Plug 'mxw/vim-jsx'
"Plug 'kchmck/vim-coffee-script'
Plug 'elixir-lang/vim-elixir'
Plug 'rhysd/vim-crystal'
"Plug 'plasticboy/vim-markdown'
"Plug 'slim-template/vim-slim'
"Plug 'tpope/vim-haml'

"" Tools
"Plug 'junegunn/goyo.vim'
Plug '907th/vim-auto-save'


call plug#end()
