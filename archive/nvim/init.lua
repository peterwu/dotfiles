require 'options'
require 'keymaps'
require 'plugins'
require 'statusline'

-- AutoSaveFolds
vim.api.nvim_create_augroup('AutoSaveFolds', {})

vim.api.nvim_create_autocmd({'BufWinLeave', 'BufLeave', 'BufWritePost'}, {
    group='AutoSaveFolds',
    pattern='?*',
    command='silent! mkview!',
    nested=true
})

vim.api.nvim_create_autocmd('BufWinEnter', {
    group='AutoSaveFolds',
    pattern='?*',
    command='silent! loadview'
})

-- AutoSetList
vim.api.nvim_create_augroup('AutoSetList', {})

vim.api.nvim_create_autocmd('InsertEnter', {
    group='AutoSetList',
    pattern='*',
    command='set list'
})

vim.api.nvim_create_autocmd('InsertLeave', {
    group='AutoSetList',
    pattern='*',
    command='set nolist'
})

-- AutoSetList
vim.api.nvim_create_augroup('HighlightedYank', {})

vim.api.nvim_create_autocmd('TextYankPost', {
    group='HighlightedYank',
    pattern='*',
    command='silent! lua vim.highlight.on_yank {timeout = 777}'
})

-- NoNumberForOldTerm
vim.api.nvim_create_augroup('NoNumberForOldTerm', {})

vim.api.nvim_create_autocmd('TermOpen', {
    group='NoNumberForOldTerm',
    pattern='*',
    command='setlocal nonumber norelativenumber'
})

-- NoTrailingWhitespaces
vim.api.nvim_create_augroup('NoTrailingWhitespaces', {})

vim.api.nvim_create_autocmd('BufWritePre', {
    group='NoTrailingWhitespaces',
    pattern='*',
    command=[[keeppatterns %s/\s\+$//e]]
})
