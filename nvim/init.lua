-- set defaults
vim.o.autochdir     = true
vim.o.completeopt   = 'menuone,noselect'
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
vim.o.timeoutlen    = 500

vim.wo.cursorline     = true
vim.wo.list           = false
vim.wo.listchars      = 'trail:·,tab:»·'
vim.wo.number         = true
vim.wo.relativenumber = true

-- assign leader keys
vim.g.mapleader      = ' '
vim.g.maplocalleader = ','

-- handle $MYVIMRC
vim.api.nvim_set_keymap('n', '<Leader>ev', [[<Cmd>edit $MYVIMRC<CR>]],   {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>sv', [[<Cmd>source $MYVIMRC<CR>]], {noremap = true, silent = true})

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

-- set list when in insert mode
vim.cmd [[autocmd InsertEnter,InsertLeave * set list!]]

vim.cmd [[
augroup AutoSaveFolds | autocmd!
  autocmd BufWinLeave,BufLeave,BufWritePost ?* nested silent! mkview!
  autocmd BufWinEnter ?* silent! loadview
augroup END

augroup Highlighted_Yank | autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="IncSearch", timeout=700}
augroup END
]]

-- tweak netrw as a file explorer
vim.g.netrw_dirhistmax   = 0
vim.g.netrw_liststyle    = 3
vim.g.netrw_banner       = 0
vim.g.netrw_browse_split = 4
vim.g.netrw_winsize      = 25
vim.g.netrw_list_hide    = [[\(^\|\s\s\)\zs\.\S\+]]
vim.g.netrw_keepdir      = 1

vim.api.nvim_set_keymap('n', '<F9>', [[<Cmd>Lexplore<CR>]], {noremap = true, silent = true})

-- tweak termdebug
vim.g.termdebug_wide = 1

-- use packer for package management
vim.cmd ('packadd packer.nvim')
require('packer').startup(function()
  use {'wbthomason/packer.nvim', opt = true, setup = function()
    vim.api.nvim_set_keymap('n', '<Leader>qc', [[<Cmd>PackerClean<CR>]],    {noremap = true, silent = true})
    vim.api.nvim_set_keymap('n', '<Leader>qi', [[<Cmd>PackerInstsall<CR>]], {noremap = true, silent = true})
    vim.api.nvim_set_keymap('n', '<Leader>qs', [[<Cmd>PackerSync<CR>]],     {noremap = true, silent = true})
    vim.api.nvim_set_keymap('n', '<Leader>qu', [[<Cmd>PackerUpdate<CR>]],   {noremap = true, silent = true})
  end}

  use {'windwp/nvim-autopairs', as = 'autopairs.nvim', config = function()
    require('nvim-autopairs').setup()
  end}

  use {'norcalli/nvim-colorizer.lua', as = 'colorizer.nvim', config = function()
    require('colorizer').setup()
  end}

  use {'hrsh7th/nvim-compe', as = 'compe.nvim', config = function()
    require('compe').setup {
      enabled          = true,
      autocomplete     = true,
      debug            = false,
      min_length       = 1,
      preselect        = 'enable',
      throttle_time    = 80,
      source_timeout   = 200,
      incomplete_delay = 400,
      max_abbr_width   = 100,
      max_kind_width   = 100,
      max_menu_width   = 100,
      documentation    = true,

      source = {
        path      = true,
        buffer    = true,
        calc      = true,
        nvim_lsp  = true,
        nvim_lua  = true,
        vsnip     = true,
        ultisnips = true
      }
    }
  end}

  use {'mattn/emmet-vim', as = 'emmet.vim'}
  use {'tommcdo/vim-exchange', as = 'exchange.vim'}
  use {'tpope/vim-eunuch', as = 'eunuch.vim'}

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

  use {'glepnir/galaxyline.nvim', config = function()
    local gl = require('galaxyline')
    local gls = gl.section
    local colors = require('modus-themes.modus_operandi').setup()

    -- left sections
    gls.left = {
      {
        FirstElement = {
          provider = function() return ' ' end,
          highlight = {colors.bg_active[1], colors.bg_active[1]}
        }
      },
      {
        ViMode = {
          provider = function()
            local mode_color = {
              n      = colors.magenta_active[1],
              i      = colors.green_active[1],
              v      = colors.cyan_active[1],
              [''] = colors.cyan_active[1],
              V      = colors.cyan_active[1],
              c      = colors.red_active[1],
              R      = colors.red_active[1],
              Rv     = colors.red_active[1],
              t      = colors.blue_active[1],
              ['!']  = colors.blue_active[1],
            }

            local alias = {
              n      = 'NORMAL',
              i      = 'INSERT',
              v      = 'VISUAL',
              [''] = 'V·BLOCK',
              V      = 'V·LINE',
              c      = 'COMMAND',
              R      = 'REPLACE',
              Rv     = 'V·REPLACE',
              t      = 'TERM',
              ['!']  = 'SHELL'
            }

            vim.cmd('highlight GalaxyViMode guifg=' .. mode_color[vim.fn.mode()])
            return alias[vim.fn.mode()]
          end,
          separator = ' ',
          separator_highlight = {colors.fg_active[1], colors.bg_active[1], 'bold'},
          highlight = {colors.fg_active[1], colors.bg_active[1], 'bold'},
        },
      },
      {
        Space = {
          provider = function() return ' ' end
        }
      },
      {
        FileName = {
          provider = 'FileName',
          condition = buffer_not_empty,
          separator = '' ,
          separator_highlight = {colors.bg_active[1], colors.bg_main[1]},
          highlight = {colors.fg_active[1], colors.bg_main[1], 'bold'}
        }
      },
      {
        GitBranch = {
          provider = 'GitBranch',
          icon = ' ',
          condition = buffer_not_empty,
          highlight = {colors.fg_dim[1], colors.bg_main[1]}
        }
      },
      {
        LeftEnd = {
          provider = function() return ' ' end,
          condition = buffer_not_empty,
          highlight = {colors.bg_active[1], colors.bg_main[1]}
        }
      }
    }

    -- right sections
    gls.right = {
      {
        FileSize = {
          provider = 'FileSize',
          condition = buffer_not_empty,
          highlight = {colors.fg_dim[1], colors.bg_main[1]}
        },
      },
      {
        FileFormat = {
          provider = 'FileFormat',
          separator = ' ',
          separator_highlight = {colors.bg_main[1], colors.bg_active[1]},
          highlight = {colors.fg_active[1], colors.bg_active[1]},
        }
      },
      {
        FileEncode = {
          provider = 'FileEncode',
          condition = buffer_not_empty,
          separator = ' |',
          separator_highlight = {colors.fg_active[1], colors.bg_active[1]},
          highlight = {colors.fg_active[1], colors.bg_active[1]},
        },
      },
      {
        PerCent = {
          provider = function()
            local fileinfo = require('galaxyline.provider_fileinfo')
            local percent = fileinfo.current_line_percent()
            return string.format('%5s', percent)
          end,
          separator = ' ',
          separator_highlight = {colors.bg_main[1], colors.bg_active[1]},
          highlight = {colors.fg_active[1], colors.bg_main[1]},
        }
      }
    }

    -- short lines
    gls.short_line_left[1] = {
      BufferType = {
        provider = 'FileName',
        separator = ' ',
        separator_highlight = {colors.bg_active[1], colors.bg_main[1]},
        highlight = {colors.fg_active[1], colors.bg_active[1]}
      }
    }

    gls.short_line_right[1] = {
      BufferIcon = {
        provider= 'BufferIcon',
        separator = ' ',
        separator_highlight = {colors.bg_active[1], colors.bg_main[1]},
        highlight = {colors.fg_active[1], colors.bg_active[1]}
      }
    }

  end}

  use {'b3nj5m1n/kommentary', as = 'kommentary.vim', config = function()
    local kommentary = require('kommentary.config')

    kommentary.use_extended_mappings()
    kommentary.configure_language('default', {
        prefer_single_line_comments = true
      })
  end}

  use {'tommcdo/vim-lion', as = 'lion.vim', config = [[vim.g.lion_squeeze_spaces = 1]]}

  use {'neovim/nvim-lspconfig', as = 'lspconfig.nvim', config = function()
    local nvim_lsp = require('lspconfig')

    local on_attach = function(client, bufnr)
      local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
      local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

      buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

      -- mappings
      local opts = {noremap=true, silent=true}

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

    nvim_lsp.clangd.setup {
      filetypes = {'c', 'cpp'},
      on_attach = on_attach
    }
  end}

  use {'ishan9299/modus-theme-vim', as = 'modus-theme.nvim', config = function()
    vim.g.modus_cursorline_intense = 1
    vim.g.modus_green_strings      = 1
    vim.g.modus_termtrans_enable   = 1
    vim.g.modus_yellow_comments    = 1

    vim.cmd('colorscheme modus-operandi')
  end}

  use {'sheerun/vim-polyglot', as = 'polyglot.vim'}
  use {'tpope/vim-repeat', as = 'repeat.vim'}
  use {'tpope/vim-sleuth', as = 'sleuth.vim'}

  use {'phaazon/hop.nvim', config = function()
    require'hop'.setup {keys = 'etovxqpdygfblzhckisuran'}

    vim.api.nvim_set_keymap('n', '<Leader>jj', [[<Cmd>HopChar1<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>jf', [[<Cmd>HopChar2<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>jw', [[<Cmd>HopWord<CR>]],  {noremap = true})
  end}

  use {'tpope/vim-surround', as = 'surround.vim'}
  
  use {'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = function()
      vim.api.nvim_set_keymap('n', '<Leader>f.', [[<Cmd>Telescope find_files<CR>]],   {noremap = true})
      vim.api.nvim_set_keymap('n', '<Leader>ff', [[<Cmd>Telescope file_browser<CR>]], {noremap = true})

      vim.api.nvim_set_keymap('n', '<Leader>f?', [[<Cmd>Telescope oldfiles<CR>]],        {noremap = true})
      vim.api.nvim_set_keymap('n', '<Leader>f:', [[<Cmd>Telescope command_history<CR>]], {noremap = true})
      vim.api.nvim_set_keymap('n', '<Leader>f/', [[<Cmd>Telescope search_history<CR>]],  {noremap = true})

      vim.api.nvim_set_keymap('n', '<Leader>fb', [[<Cmd>Telescope buffers<CR>]],  {noremap = true})
      vim.api.nvim_set_keymap('n', '<Leader>fh', [[<Cmd>Telescope help_tags<CR>]], {noremap = true})
      vim.api.nvim_set_keymap('n', '<Leader>fm', [[<Cmd>Telescope keymaps<CR>]],     {noremap = true})
      vim.api.nvim_set_keymap('n', '<Leader>ft', [[<Cmd>Telescope tags<CR>]],     {noremap = true})
    end}

  use {'tpope/vim-unimpaired', as = 'unimpaired.vim'}

  use {'tpope/vim-vinegar', as = 'vinegar.vim', config = function()
    vim.api.nvim_set_keymap('n', '-', [[k^]], {noremap = true})
  end}

  use {'folke/which-key.nvim', config = function()
    require("which-key").setup {}
  end}

end)

