let g:deoplete#enable_at_startup = 1
let g:deoplete#omni#input_patterns = {}
let g:deoplete#omni#input_patterns.ruby = 
      \ ['[^. *\t]\.\w*', '[a-zA-Z]w*::']

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ deoplete#mappings#manual_complete()
