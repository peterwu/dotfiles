-- assign leader keys
vim.g.mapleader      = ' '
vim.g.maplocalleader = ','

-- handle $MYVIMRC
vim.api.nvim_set_keymap('n', '<Leader>ev', [[<Cmd>edit $MYVIMRC<CR>]],   {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>sv', [[<Cmd>source $MYVIMRC<CR>]], {noremap = true, silent = true})

-- change cwd
vim.api.nvim_set_keymap('n', '<Leader>cd', [[<Cmd>chdir %:p:h<CR><Cmd>pwd<CR>]], {noremap = true, silent = true})

-- disable arrow keys
vim.api.nvim_set_keymap('', '<Up>',    [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('', '<Down>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('', '<Left>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('', '<Right>', [[<Nop>]], {noremap = true, silent = true})

vim.api.nvim_set_keymap('i', '<Up>',    [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('i', '<Down>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('i', '<Left>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('i', '<Right>', [[<Nop>]], {noremap = true, silent = true})

-- swap j/k <-> gj/gk
vim.api.nvim_set_keymap('n', 'j', [[(v:count? 'j' : 'gj')]], {noremap = true, expr = true})
vim.api.nvim_set_keymap('n', 'k', [[(v:count? 'k' : 'gk')]], {noremap = true, expr = true})

-- copy to clipboard
vim.api.nvim_set_keymap('n', 'Y',          [[y$]],   {noremap = true})
vim.api.nvim_set_keymap('v', '<Leader>y',  [["+y]],  {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>y',  [["+y]],  {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>Y',  [["+y$]], {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>yy', [["+yy]], {noremap = true})

-- paste from clipboard
vim.api.nvim_set_keymap('n', '<Leader>p', [["+p]], {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>P', [["+P]], {noremap = true})
vim.api.nvim_set_keymap('v', '<Leader>p', [["+p]], {noremap = true})
vim.api.nvim_set_keymap('v', '<Leader>P', [["+P]], {noremap = true})

-- manage tabs
vim.api.nvim_set_keymap('n', '<C-Left>',  [[<Cmd>tabfirst<CR>]],   {noremap = true})
vim.api.nvim_set_keymap('n', '<C-Right>', [[<Cmd>tablast<CR>]],    {noremap = true})
vim.api.nvim_set_keymap('n', '<A-Left>',  [[<Cmd>tabmove -1<CR>]], {noremap = true})
vim.api.nvim_set_keymap('n', '<A-Right>', [[<Cmd>tabmove +1<CR>]], {noremap = true})

-- <Ctrl-L> redraws the screen and removes any search highlighting
vim.api.nvim_set_keymap('n', '<C-l>', [[<Cmd>nohlsearch<CR><C-l>]], {noremap = true, silent = true})

-- launch terminal
vim.api.nvim_set_keymap('n', '<Leader>o', [[<Cmd>below 10sp term://$SHELL<CR>i]], {noremap = true, silent = true})
