-- defaults
vim.o.background    = 'light'
vim.o.hidden        = true
vim.o.ignorecase    = true
vim.o.inccommand    = 'nosplit'
vim.o.lazyredraw    = true
vim.o.mouse         = 'a'
vim.o.path          = vim.o.path .. '**'
vim.o.shortmess     = vim.o.shortmess .. 'I'
vim.o.showmatch     = true
vim.o.showmode      = false
vim.o.smartcase     = true
vim.o.termguicolors = true

vim.wo.cursorline     = true
vim.wo.list           = false
vim.wo.listchars      = 'trail:·,tab:»·'
vim.wo.number         = true
vim.wo.relativenumber = true

-- key mappings
vim.g.mapleader      = " "
vim.g.maplocalleader = ","

vim.api.nvim_set_keymap('n', '<Leader>cd', [[<Cmd>cd %:p:h<CR>:pwd<CR>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>ev', [[<Cmd>edit $MYVIMRC<CR>]],    {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>sv', [[<Cmd>source $MYVIMRC<CR>]],  {noremap = true, silent = true})

vim.api.nvim_set_keymap('n', '<Leader>pc', [[<Cmd>PackerClean<CR>]],    {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>pi', [[<Cmd>PackerInstsall<CR>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>ps', [[<Cmd>PackerSync<CR>]],     {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>pu', [[<Cmd>PackerUpdate<CR>]],   {noremap = true, silent = true})

-- disable arrow keys

vim.api.nvim_set_keymap('n', '<Up>',    [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Down>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Left>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Right>', [[<Nop>]], {noremap = true, silent = true})

vim.api.nvim_set_keymap('i', '<Up>',    [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('i', '<Down>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('i', '<Left>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('i', '<Right>', [[<Nop>]], {noremap = true, silent = true})

vim.api.nvim_set_keymap('v', '<Up>',    [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('v', '<Down>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('v', '<Left>',  [[<Nop>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('v', '<Right>', [[<Nop>]], {noremap = true, silent = true})

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

-- <Ctrl-L> redraws the screen and removes any search highlighting
vim.api.nvim_set_keymap('n', '<C-L>', [[<Cmd>nohlsearch<CR><C-L>]], {noremap = true, silent = true})

-- terminal
vim.api.nvim_set_keymap('n', '<Leader>o', [[<Cmd>below 10sp term://$SHELL<CR>i]], {noremap = true, silent = true})

vim.cmd [[
" set list when in insert mode
autocmd InsertEnter,InsertLeave * set list!

" initialization
" set list when in insert mode
autocmd InsertEnter,InsertLeave * set list!

augroup AutoSaveFolds | autocmd!
  autocmd BufWinLeave,BufLeave,BufWritePost ?* nested silent! mkview!
  autocmd BufWinEnter ?* silent! loadview
augroup END
]]

-- " builtin plugin settings "
-- netrw
vim.g.netrw_dirhistmax   = 0
vim.g.netrw_liststyle    = 3
vim.g.netrw_banner       = 0
vim.g.netrw_browse_split = 4
vim.g.netrw_winsize      = 25
vim.g.netrw_list_hide    = [[ \(^\|\s\s\)\zs\.\S\+ ]]
vim.g.netrw_keepdir      = 1

vim.api.nvim_set_keymap('n', '<F9>', [[<Cmd>Lexplore<CR>]], {noremap = true, silent = true})

-- termdebug
vim.g.termdebug_wide = 1

-- packer
vim.cmd ('packadd packer.nvim')
require'packer'.startup(function()
  use {'wbthomason/packer.nvim', opt = true}

  use {'tpope/vim-sleuth', as = 'sleuth.vim'}

  use {'jiangmiao/auto-pairs', as = 'auto-pairs.vim'}
  use {'tpope/vim-commentary', as = 'commentary.vim'}
  use {'machakann/vim-highlightedyank', as = 'highlighted-yank.vim'}
  use {'tpope/vim-repeat', as = 'repeat.vim'}
  use {'tpope/vim-surround', as = 'surround.vim'}
  use {'tpope/vim-unimpaired', as = 'unimpaired.vim'}
  use {'justinmk/vim-sneak', as = 'sneak.vim', config = [[ vim.g['sneak#label'] = 1 ]]}
  use {'tommcdo/vim-exchange', as = 'exchange.vim'}
  use {'tommcdo/vim-lion', as = 'lion.vim', config = [[ vim.g.lion_squeeze_spaces = 1 ]]}

  use {'tpope/vim-fugitive', as = 'fugitive.vim', config = function()

    vim.api.nvim_set_keymap('n', '<Leader>gs', [[<Cmd>Git<CR>]],       {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gb', [[<Cmd>Git blame<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gr', [[<Cmd>Gread<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gw', [[<Cmd>Gwrite<CR>]],    {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gl', [[<Cmd>Gclog<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gp', [[<Cmd>Git push<CR>]],  {noremap = true})

    -- diff / merge
    vim.api.nvim_set_keymap('n', '<Leader>gd', [[<Cmd>Gvdiffsplit<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', 'gdh',        [[<Cmd>diffget //2<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', 'gdl',        [[<Cmd>diffget //3<CR>]], {noremap = true})
  end}

  use {'tpope/vim-rhubarb', as = 'rhubarb.vim'}

  use {'tpope/vim-eunuch', as = 'eunuch.vim'}

  use {'tpope/vim-vinegar', as = 'vinegar.vim', config = function()
    vim.api.nvim_set_keymap('n', '-', [[k^]], {noremap = true})
  end}

  use {'mattn/emmet-vim', as = 'emmet.vim'}

  use {'junegunn/fzf.vim', as = 'fzf.vim', config = function()
    vim.api.nvim_set_keymap('n', '<Leader>f.', [[<Cmd>Files .<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>f`', [[<Cmd>Files /<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>f~', [[<Cmd>Files $HOME<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>f%', [[<Cmd>Files %:p:h<CR>]], {noremap = true})

    vim.api.nvim_set_keymap('n', '<Leader>f?', [[<Cmd>History<CR>]],  {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>f:', [[<Cmd>History:<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>f/', [[<Cmd>History/<CR>]], {noremap = true})

    vim.api.nvim_set_keymap('n', '<Leader>fb', [[<Cmd>Buffers<CR>]],  {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>fh', [[<Cmd>Helptags<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>fm', [[<Cmd>Maps<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>ft', [[<Cmd>Tags<CR>]],     {noremap = true})
  end}

  use {'sheerun/vim-polyglot', as = 'polyglot.vim'}

  use {'chrisbra/colorizer', as = 'colorizer.vim'}

  use {'itchyny/lightline.vim', as = 'lightline.vim', config = function()
    vim.g.lightline = { colorscheme = 'solarized', 
      active = { 
        left = { 
          { 'mode', 'paste' }, 
        { 'gitbranch', 'readonly', 'filename', 'modified' } }
      },
      component_function = {
        gitbranch = 'FugitiveHead'
      }
    }
  end}

  use {'ishan9299/modus-theme-vim', as = 'modus-theme.nvim', config = function()
    vim.g.modus_cursorline_intense = 1
    vim.g.modus_green_strings      = 1
    vim.g.modus_termtrans_enable   = 1
    vim.g.modus_yellow_comments    = 1

    vim.cmd('colorscheme modus-operandi') -- Light
  end}

  use {'neovim/nvim-lspconfig', as = 'lspconfig.nvim', config = function()
    local nvim_lsp = require'lspconfig'

    local on_attach = function(client, bufnr)
      local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
      local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

      buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

      -- Mappings.
      local opts = { noremap=true, silent=true }

      buf_set_keymap('n', 'gD',        [[<Cmd>lua vim.lsp.buf.declaration()<CR>]],                                opts)
      buf_set_keymap('n', 'gd',        [[<Cmd>lua vim.lsp.buf.definition()<CR>]],                                 opts)
      buf_set_keymap('n', 'K',         [[<Cmd>lua vim.lsp.buf.hover()<CR>]],                                      opts)
      buf_set_keymap('n', 'gi',        [[<Cmd>lua vim.lsp.buf.implementation()<CR>]],                             opts)
      buf_set_keymap('n', '<C-k>',     [[<Cmd>lua vim.lsp.buf.signature_help()<CR>]],                             opts)
      buf_set_keymap('n', '<space>wa', [[<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>]],                       opts)
      buf_set_keymap('n', '<space>wr', [[<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>]],                    opts)
      buf_set_keymap('n', '<space>wl', [[<Cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>]], opts)
      buf_set_keymap('n', '<space>D',  [[<Cmd>lua vim.lsp.buf.type_definition()<CR>]],                            opts)
      buf_set_keymap('n', '<space>rn', [[<cmd>lua vim.lsp.buf.rename()<CR>]],                                     opts)
      buf_set_keymap('n', 'gr',        [[<Cmd>lua vim.lsp.buf.references()<CR>]],                                 opts)
      buf_set_keymap('n', '<space>e',  [[<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>]],               opts)
      buf_set_keymap('n', '[d',        [[<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>]],                           opts)
      buf_set_keymap('n', ']d',        [[<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>]],                           opts)
      buf_set_keymap('n', '<space>q',  [[<Cmd>lua vim.lsp.diagnostic.set_loclist()<CR>]],                         opts)

    end

    nvim_lsp.clangd.setup{
      filetypes = {"c", "cpp"};
      on_attach = on_attach
    }
  end}

  use {'ajh17/VimCompletesMe', as = 'vcm.vim'}

end)

