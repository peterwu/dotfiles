-- use packer for package management
vim.cmd [[packadd packer.nvim]]
require 'packer'.startup {function()
  use {'wbthomason/packer.nvim', opt = true, setup = function()
    require 'plugins.packer'.setup()
  end}

  use {'windwp/nvim-autopairs', as = 'autopairs.nvim', config = function()
    require 'nvim-autopairs'.setup()
  end}

  use {'norcalli/nvim-colorizer.lua', as = 'colorizer.nvim', config = function()
    require 'colorizer'.setup()
  end}

  use {'tpope/vim-commentary', as = 'commentary.vim'}

  use {'hrsh7th/nvim-compe', as = 'compe.nvim', config = function()
    require 'plugins.compe'.setup()
  end}

  use {'mfussenegger/nvim-dap', as = 'dap.nvim', config = function()
    require 'plugins.dap'.setup()
  end}

  use {"rcarriga/nvim-dap-ui", as = 'dap-ui.nvim', config = function()
    require 'dapui'.setup()
  end}

  use {'tommcdo/vim-exchange', as = 'exchange.vim'}

  use {'phaazon/hop.nvim', config = function()
    require 'plugins.hop'.setup()
  end}

  use {'tommcdo/vim-lion', as = 'lion.vim', config = function()
    vim.g.lion_squeeze_spaces = 1
  end}

  use {'neovim/nvim-lspconfig', as = 'lspconfig.nvim', config = function()
    require 'plugins.lspconfig'.setup()
  end}
   
  use {'ishan9299/modus-theme-vim', as = 'modus-theme.nvim', config = function()
    require 'plugins.modus-theme'.setup()
  end}

  use {'kristijanhusak/orgmode.nvim', config = function()
    require 'orgmode'.setup()
  end}

  use {'tpope/vim-repeat', as = 'repeat.vim'}

  use {'dstein64/nvim-scrollview', as = 'scrollview.nvim'}

  use {'tpope/vim-surround', as = 'surround.vim'}

  use {'nvim-telescope/telescope.nvim',
  requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
  config = function()
    require 'plugins.telescope'.setup()
  end}

  use {'nvim-telescope/telescope-dap.nvim', config = function()
    require 'plugins.telescope-dap'.setup()
  end}

  use {'nvim-treesitter/nvim-treesitter', as = 'tree-sitter.nvim', run = ':TSUpdate'}

  use {'tpope/vim-unimpaired', as = 'unimpaired.vim'}
  
  use {'tpope/vim-vinegar', as = 'vinegar.vim', config = function()
    require 'plugins.vinegar'.setup()
  end}

  use {'folke/which-key.nvim', config = function()
    require 'which-key'.setup()
  end}

end,
config = {
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'single' })
    end
  }
}}
