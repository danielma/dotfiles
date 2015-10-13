au BufNewFile,BufRead *.jsx set filetype=javascript.jsx
au BufNewFile,BufRead *.js set filetype=javascript.jsx
autocmd BufRead,BufNewFile *.es6 setfiletype javascript

let g:jsx_ext_required = 0
