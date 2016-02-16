function! SetPluginOptionsNow()
  autocmd! BufWritePost * Neomake
endfunction

au VimEnter * call SetPluginOptionsNow()
