-- use packer for package management
vim.cmd ('packadd packer.nvim')
require('packer').startup {function()
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

  use {'tpope/vim-commentary', as = 'commentary.vim'}

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

  use {'mfussenegger/nvim-dap', as = 'dap.nvim', config = function()
    local dap = require('dap')
    dap.adapters.lldb = {
      type = 'executable',
      command = '/usr/bin/lldb-vscode',
      name = 'lldb'
    }

    dap.configurations.c = {
      {
        name = 'Launch',
        type = 'lldb',
        request = 'launch',
        program = function()
          return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
        args = {},
        runInTerminal = true,
      },
    }

    dap.configurations.cpp = dap.configurations.c
    dap.configurations.rust = dap.configurations.cpp

    local opts = {noremap=true, silent=true}

    vim.api.nvim_set_keymap('n', '<F5>',   [[<Cmd>lua require('dap').continue()<CR>]],          opts)
    vim.api.nvim_set_keymap('n', '<F8>',   [[<Cmd>lua require('dap').step_over()<CR>]],         opts)
    vim.api.nvim_set_keymap('n', '<F7>',   [[<Cmd>lua require('dap').into()<CR>]],              opts)
    vim.api.nvim_set_keymap('n', '<S-F7>', [[<Cmd>lua require('dap').out()<CR>]],               opts)
    vim.api.nvim_set_keymap('n', '<F6>',   [[<Cmd>lua require('dap').toggle_breakpoint()<CR>]], opts)
    vim.api.nvim_set_keymap('n', '<F10>',  [[<Cmd>lua require('dap').repl.open()<CR>]],         opts)
  end}

  use {"rcarriga/nvim-dap-ui", as = 'dap-ui.nvim', config = function()
    require("dapui").setup()
  end}

  use {'mattn/emmet-vim', as = 'emmet.vim'}

  use {'tommcdo/vim-exchange', as = 'exchange.vim'}

  use {'tpope/vim-fugitive', as = 'fugitive.vim', config = function()
    vim.api.nvim_set_keymap('n', '<Leader>gs', [[<Cmd>Git<CR>]],       {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gb', [[<Cmd>Git blame<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gr', [[<Cmd>Gread<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gw', [[<Cmd>Gwrite<CR>]],    {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gl', [[<Cmd>Gclog<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gp', [[<Cmd>Git push<CR>]],  {noremap = true})

    -- diff / merge
    vim.api.nvim_set_keymap('n', '<Leader>gdd', [[<Cmd>Gvdiffsplit<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gdh', [[<Cmd>diffget //2<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>gdl', [[<Cmd>diffget //3<CR>]], {noremap = true})
  end}

  use {'glepnir/galaxyline.nvim', config = function()
    local gl = require('galaxyline')
    local gls = gl.section

    local colors = {
      bg_main                 = "#ffffff",
      fg_main                 = "#000000",
      bg_dim                  = "#f8f8f8",
      fg_dim                  = "#282828",
      bg_alt                  = "#f0f0f0",
      fg_alt                  = "#505050",
      bg_active               = "#d7d7d7",
      fg_active               = "#0a0a0a",
      bg_inactive             = "#efefef",
      fg_inactive             = "#404148",
      red                     = "#a60000",
      red_alt                 = "#972500",
      red_alt_other           = "#a0132f",
      red_faint               = "#7f1010",
      red_alt_faint           = "#702f00",
      red_alt_other_faint     = "#7f002f",
      green                   = "#005e00",
      green_alt               = "#315b00",
      green_alt_other         = "#145c33",
      green_faint             = "#104410",
      green_alt_faint         = "#30440f",
      green_alt_other_faint   = "#0f443f",
      yellow                  = "#813e00",
      yellow_alt              = "#70480f",
      yellow_alt_other        = "#863927",
      yellow_faint            = "#5f4400",
      yellow_alt_faint        = "#5d5000",
      yellow_alt_other_faint  = "#5e3a20",
      blue                    = "#0031a9",
      blue_alt                = "#2544bb",
      blue_alt_other          = "#0000c0",
      blue_faint              = "#003497",
      blue_alt_faint          = "#0f3d8c",
      blue_alt_other_faint    = "#001087",
      magenta                 = "#721045",
      magenta_alt             = "#8f0075",
      magenta_alt_other       = "#5317ac",
      magenta_faint           = "#752f50",
      magenta_alt_faint       = "#7b206f",
      magenta_alt_other_faint = "#55348e",
      cyan                    = "#00538b",
      cyan_alt                = "#30517f",
      cyan_alt_other          = "#005a5f",
      cyan_faint              = "#005077",
      cyan_alt_faint          = "#354f6f",
      cyan_alt_other_faint    = "#125458",
      red_intense             = "#b60000",
      orange_intense          = "#904200",
      green_intense           = "#006800",
      yellow_intense          = "#605b00",
      blue_intense            = "#1f1fce",
      magenta_intense         = "#a8007f",
      purple_intense          = "#7f10d0",
      cyan_intense            = "#005f88",
      red_active              = "#8a0000",
      green_active            = "#004c2e",
      yellow_active           = "#702d1f",
      blue_active             = "#0030b4",
      magenta_active          = "#5c2092",
      cyan_active             = "#003f8a"
    }

    -- left sections
    gls.left = {
      {
        FirstElement = {
          provider = function() return ' ' end,
          highlight = {colors.bg_active, colors.bg_active}
        }
      },
      {
        ViMode = {
          provider = function()
            local mode_color = {
              n      = colors.magenta_active,
              i      = colors.green_active,
              v      = colors.cyan_active,
              [''] = colors.cyan_active,
              V      = colors.cyan_active,
              c      = colors.red_active,
              R      = colors.red_active,
              Rv     = colors.red_active,
              t      = colors.blue_active,
              ['!']  = colors.blue_active
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
          separator_highlight = {colors.fg_active, colors.bg_active, 'bold'},
          highlight = {colors.fg_active, colors.bg_active, 'bold'},
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
          separator_highlight = {colors.bg_active, colors.bg_main},
          highlight = {colors.fg_active, colors.bg_main, 'bold'}
        }
      },
      {
        GitBranch = {
          provider = 'GitBranch',
          icon = ' ',
          condition = buffer_not_empty,
          highlight = {colors.fg_dim, colors.bg_main}
        }
      },
      {
        LeftEnd = {
          provider = function() return ' ' end,
          condition = buffer_not_empty,
          highlight = {colors.bg_active, colors.bg_main}
        }
      }
    }

    -- right sections
    gls.right = {
      {
        FileSize = {
          provider = 'FileSize',
          condition = buffer_not_empty,
          highlight = {colors.fg_dim, colors.bg_main}
        },
      },
      {
        FileFormat = {
          provider = 'FileFormat',
          separator = ' ',
          separator_highlight = {colors.bg_main, colors.bg_active},
          highlight = {colors.fg_active, colors.bg_active},
        }
      },
      {
        FileEncode = {
          provider = 'FileEncode',
          condition = buffer_not_empty,
          separator = ' |',
          separator_highlight = {colors.fg_active, colors.bg_active},
          highlight = {colors.fg_active, colors.bg_active},
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
          separator_highlight = {colors.bg_main, colors.bg_active},
          highlight = {colors.fg_active, colors.bg_main},
        }
      }
    }

    -- short lines
    gls.short_line_left[1] = {
      BufferType = {
        provider = 'FileName',
        separator = ' ',
        separator_highlight = {colors.bg_active, colors.bg_main},
        highlight = {colors.fg_active, colors.bg_active}
      }
    }

    gls.short_line_right[1] = {
      BufferIcon = {
        provider = 'BufferIcon',
        separator = ' ',
        separator_highlight = {colors.bg_active, colors.bg_main},
        highlight = {colors.fg_active, colors.bg_active}
      }
    }
  end}

  use {'phaazon/hop.nvim', config = function()
    require('hop').setup {keys = 'etovxqpdygfblzhckisuran'}

    vim.api.nvim_set_keymap('n', '<Leader>jj', [[<Cmd>HopChar1<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>jf', [[<Cmd>HopChar2<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>jw', [[<Cmd>HopWord<CR>]],  {noremap = true})
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

      buf_set_keymap('n', 'gD',         [[<Cmd>lua vim.lsp.buf.declaration()<CR>]],                                opts)
      buf_set_keymap('n', 'gd',         [[<Cmd>lua vim.lsp.buf.definition()<CR>]],                                 opts)
      buf_set_keymap('n', 'K',          [[<Cmd>lua vim.lsp.buf.hover()<CR>]],                                      opts)
      buf_set_keymap('n', 'gi',         [[<Cmd>lua vim.lsp.buf.implementation()<CR>]],                             opts)
      buf_set_keymap('n', '<C-k>',      [[<Cmd>lua vim.lsp.buf.signature_help()<CR>]],                             opts)
      buf_set_keymap('n', '<Leader>wa', [[<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>]],                       opts)
      buf_set_keymap('n', '<Leader>wr', [[<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>]],                    opts)
      buf_set_keymap('n', '<Leader>wl', [[<Cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>]], opts)
      buf_set_keymap('n', '<Leader>D',  [[<Cmd>lua vim.lsp.buf.type_definition()<CR>]],                            opts)
      buf_set_keymap('n', '<Leader>rn', [[<cmd>lua vim.lsp.buf.rename()<CR>]],                                     opts)
      buf_set_keymap('n', 'gr',         [[<Cmd>lua vim.lsp.buf.references()<CR>]],                                 opts)
      buf_set_keymap('n', '<Leader>e',  [[<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>]],               opts)
      buf_set_keymap('n', '[d',         [[<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>]],                           opts)
      buf_set_keymap('n', ']d',         [[<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>]],                           opts)
      buf_set_keymap('n', '<Leader>q',  [[<Cmd>lua vim.lsp.diagnostic.set_loclist()<CR>]],                         opts)
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

    vim.cmd [[colorscheme modus-operandi]]
  end}

  use {'kristijanhusak/orgmode.nvim', config = function()
    require('orgmode').setup()
  end}

  use {'tpope/vim-repeat', as = 'repeat.vim'}

  use {'dstein64/nvim-scrollview', as = 'scrollview.nvim'}

  use {'tpope/vim-surround', as = 'surround.vim'}

  use {'nvim-telescope/telescope.nvim',
  requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
  config = function()
    vim.api.nvim_set_keymap('n', '<Leader>ff', [[<Cmd>Telescope find_files<CR>]],   {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>fo', [[<Cmd>Telescope oldfiles<CR>]],     {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>fx', [[<Cmd>Telescope file_browser<CR>]], {noremap = true})

    vim.api.nvim_set_keymap('n', '<Leader>f:', [[<Cmd>Telescope command_history<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>f/', [[<Cmd>Telescope search_history<CR>]],  {noremap = true})

    vim.api.nvim_set_keymap('n', '<Leader>fb', [[<Cmd>Telescope buffers<CR>]],   {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>fh', [[<Cmd>Telescope help_tags<CR>]], {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>fm', [[<Cmd>Telescope keymaps<CR>]],   {noremap = true})
    vim.api.nvim_set_keymap('n', '<Leader>ft', [[<Cmd>Telescope tags<CR>]],      {noremap = true})
  end}

  use {'nvim-telescope/telescope-dap.nvim', config = function()
    require('telescope').load_extension('dap')

    local opts = {noremap=true, silent=true}

    vim.api.nvim_set_keymap('n', '<Leader>fdd', [[<Cmd>Telescope dap commands<CR>]],         opts)
    vim.api.nvim_set_keymap('n', '<Leader>fdc', [[<Cmd>Telescope dap configurations<CR>]],   opts)
    vim.api.nvim_set_keymap('n', '<Leader>fdb', [[<Cmd>Telescope dap list_breakpoints<CR>]], opts)
    vim.api.nvim_set_keymap('n', '<Leader>fdv', [[<Cmd>Telescope dap variables<CR>]],        opts)
    vim.api.nvim_set_keymap('n', '<Leader>fdf', [[<Cmd>Telescope dap frames<CR>]],           opts)
  end}

  use {'nvim-treesitter/nvim-treesitter', as = 'tree-sitter.nvim', run = ':TSUpdate'}

  use {'kyazdani42/nvim-tree.lua', as = 'tree.nvim', config = function()
    vim.g.nvim_tree_add_trailing           = 1
    vim.g.nvim_tree_follow                 = 1
    vim.g.nvim_tree_git_hl                 = 1
    vim.g.nvim_tree_hide_dotfiles          = 1
    vim.g.nvim_tree_highlight_opened_files = 2
    vim.g.nvim_tree_indent_markers         = 1
    vim.g.nvim_tree_quit_on_open           = 1
    vim.g.nvim_tree_special_files          = {}
    vim.g.nvim_tree_update_cwd             = 1
    vim.g.nvim_tree_width                  = 37

    vim.api.nvim_set_keymap('n', '<F9>', [[<Cmd>NvimTreeToggle<CR>]], {noremap = true, silent = true})
  end}

  use {'tpope/vim-unimpaired', as = 'unimpaired.vim'}

  use {'folke/which-key.nvim', config = function()
    require("which-key").setup {}
  end}

end,
config = {
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'single' })
    end
  }
}
}
