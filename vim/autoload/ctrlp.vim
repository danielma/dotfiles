let g:ctrlp_custom_ignore = { 'dir' : '\v(node_modules|bower_components|dist|deps|_build|tmp)$' }

let g:ctrlp_root_markers = ['.project']
let g:ctrlp_switch_buffer = 0

let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_use_caching = 0

imap <C-P> <Esc><C-P>
map <Leader>p <Esc><C-P>

