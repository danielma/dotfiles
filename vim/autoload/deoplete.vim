let g:deoplete#enable_at_startup = 1
" let g:deoplete#disable_auto_complete = 1
"let g:deoplete#auto_completion_start_length = 3
"let g:deoplete#omni#input_patterns = {}
"let g:deoplete#omni#input_patterns.ruby = 
"      \ ['[^. *\t]\.\w*', '[a-zA-Z]w*::']

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ deoplete#mappings#manual_complete()
