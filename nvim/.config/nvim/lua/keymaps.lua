local utils = require('utils')

-- assign leader keys
vim.g.mapleader      = ' '
vim.g.maplocalleader = ','

-- handle $MYVIMRC
utils.nmap('<Leader>ev', [[<Cmd>edit $MYVIMRC<CR>]])
utils.nmap('<Leader>sv', [[<Cmd>source $MYVIMRC<CR>]])

-- change cwd
utils.nmap('<Leader>cd', [[<Cmd>chdir %:p:h<CR><Cmd>pwd<CR>]])

-- disable arrow keys
utils.map('<Up>',    [[<Nop>]])
utils.map('<Down>',  [[<Nop>]])
utils.map('<Left>',  [[<Nop>]])
utils.map('<Right>', [[<Nop>]])

utils.imap('<Up>',    [[<Nop>]])
utils.imap('<Down>',  [[<Nop>]])
utils.imap('<Left>',  [[<Nop>]])
utils.imap('<Right>', [[<Nop>]])

-- swap j/k <-> gj/gk
utils.nmap('j', [[v:count ? 'j' : 'gj']], {expr = true})
utils.nmap('k', [[v:count ? 'k' : 'gk']], {expr = true})

-- copy to clipboard
utils.nmap('Y',          [[yg_]])
utils.vmap('<Leader>y',  [["+y]])
utils.nmap('<Leader>y',  [["+y]])
utils.nmap('<Leader>Y',  [["+yg_]])
utils.nmap('<Leader>yy', [["+yy]])

-- paste from clipboard
utils.nmap('<Leader>p', [["+p]])
utils.nmap('<Leader>P', [["+P]])
utils.vmap('<Leader>p', [["+p]])
utils.vmap('<Leader>P', [["+P]])

-- manage tabs
utils.nmap('<C-Left>',  [[<Cmd>tabfirst<CR>]])
utils.nmap('<C-Right>', [[<Cmd>tablast<CR>]])
utils.nmap('<A-Left>',  [[<Cmd>tabmove -1<CR>]])
utils.nmap('<A-Right>', [[<Cmd>tabmove +1<CR>]])

-- <Ctrl-L> redraws the screen and removes any search highlighting
utils.nmap('<C-L>', [[<Cmd>nohlsearch<CR><C-L>]])

-- use tab to select from popup menu
utils.imap('<Tab>',   [[pumvisible() ? '<C-N>' : '<Tab>']], {expr = true})
utils.imap('<S-Tab>', [[pumvisible() ? '<C-P>' : '<C-H>']], {expr = true})

-- launch terminal
utils.nmap('<Leader>o', [[<Cmd>below 10sp term://$SHELL<CR>i]])

-- diff / merge
utils.nmap('gdb', [[<Cmd>diffget BA<CR>]])
utils.nmap('gdl', [[<Cmd>diffget LO<CR>]])
utils.nmap('gdr', [[<Cmd>diffget RE<CR>]])
