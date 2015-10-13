"let g:%{lightline#link()}%#LightLineLeft_active_0#%( %{lightline#mode()} %)%{(1)*((&paste))?('|'):''}%( %{&paste?"PASTE":""} %)%#LightLineLeft_active_0_1#%#LightLineLeft_active_1#%( %R %)%{(&readonly)*((1)+(&modified||!&modifiable))?('|'):''}%( %t %)%{(1)*((&modified||!&modifiable))?('|'):''}%( %M %)%#LightLineLeft_active_1_2#%#LightLineMiddle_active#%=%#LightLineRight_active_2_3#|%#LightLineRight_active_2#%( %{&fileformat} %)%
"{(1)*((1))?('|'):''}%( %{strlen(&fenc)?&fenc:&enc} %)%{(1)*((1)+(1))?('|'):''}%( %{strlen(&filetype)?&filetype:"no ft"} %)%#LightLineRight_active_1_2#%#LightLineRight_active_1#%( %3p%% %)%#LightLineRight_active_0_1#%#LightLineRight_active_0#%( %3l:%-2v %)

let g:lightline = {
      \ 'colorscheme': 'Tomorrow_Night',
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"⦼":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}'
      \ },
      \ 'component_expand': {
      \   'neomake': 'NeomakeStatus'
      \ },
      \ 'component_function': {
      \   'neomake': 'NeomakeStatus'
      \ },
      \ 'component_type': {
      \   'neomake': 'error',
      \ },
      \ 'active': {
      \   'right': [ [ 'neomake', 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'filetype' ] ],
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ] ]
      \ },
      \ }

function! NeomakeStatus()
  return neomake#statusline#LoclistStatus()
endfunc
