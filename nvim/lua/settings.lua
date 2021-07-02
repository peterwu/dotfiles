-- set options
vim.opt.completeopt    = 'menuone,noselect'
vim.opt.cursorline     = true
vim.opt.expandtab      = true
vim.opt.hidden         = true
vim.opt.ignorecase     = true
vim.opt.inccommand     = 'nosplit'
vim.opt.lazyredraw     = true
vim.opt.list           = false
vim.opt.listchars      = {trail = '·', tab = '»·'}
vim.opt.mouse          = 'a'
vim.opt.number         = true
vim.opt.path           = vim.opt.path + '**'
vim.opt.pumheight      = 7
vim.opt.relativenumber = true
vim.opt.sessionoptions = 'folds'
vim.opt.shiftwidth     = 2
vim.opt.shortmess      = vim.opt.shortmess + 'Ic'
vim.opt.showmatch      = true
vim.opt.showmode       = false
vim.opt.smartcase      = true
vim.opt.smartindent    = true
vim.opt.softtabstop    = 2
vim.opt.splitbelow     = true
vim.opt.splitright     = true
vim.opt.tabstop        = 2
vim.opt.termguicolors  = true
vim.opt.timeoutlen     = 777
vim.opt.title          = true
vim.opt.undofile       = true
vim.opt.viewoptions    = 'cursor,folds'

-- tweak termdebug
vim.g.termdebug_wide = 1
