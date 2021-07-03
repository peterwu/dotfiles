require 'options'
require 'keymaps'
require 'plugins'
require 'statusline'

vim.cmd [[
augroup AutoSaveFolds | autocmd!
  autocmd BufWinLeave,BufLeave,BufWritePost ?* nested silent! mkview!
  autocmd BufWinEnter ?* silent! loadview
augroup END

augroup AutoSetList | autocmd!
  autocmd InsertEnter * set list
  autocmd InsertLeave * set nolist
augroup END

augroup HighlightedYank | autocmd!
  autocmd TextYankPost * silent! lua vim.highlight.on_yank {timeout = 777}
augroup END

augroup NoNumberForOldTerm | autocmd!
  autocmd TermOpen * setlocal nonumber norelativenumber
augroup END

augroup NoTrailingWhitespaces | autocmd!
  autocmd BufWritePre * :%s/\s\+$//e
augroup END
]]
