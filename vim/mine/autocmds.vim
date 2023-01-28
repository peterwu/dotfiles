" -------------
" auto commands
" -------------
augroup AutoSaveFolds | autocmd!
    autocmd BufWinLeave,BufLeave,BufWritePost ?* ++nested silent! mkview!
    autocmd BufWinEnter ?* silent! loadview
augroup END

augroup AutoSetList | autocmd!
    autocmd InsertEnter,InsertLeave * set list!
augroup END

augroup NoNumberForOldTerm | autocmd!
    autocmd TerminalWinOpen * setlocal nonumber norelativenumber
augroup END

augroup NoTrailingWhitespaces | autocmd!
    autocmd BufWritePre * :%s/\s\+$//e
augroup END
