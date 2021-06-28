require 'settings'
require 'mappings'
require 'plugins'

vim.cmd [[
augroup AutoSaveFolds | autocmd!
  autocmd BufWinLeave,BufLeave,BufWritePost *.* nested silent! mkview!
  autocmd BufWinEnter *.* silent! loadview
augroup END

augroup AutoSetList | autocmd!
  autocmd InsertEnter * set list
  autocmd InsertLeave * set nolist
augroup END

augroup HighlightedYank | autocmd!
  autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=777}
augroup END
]]
