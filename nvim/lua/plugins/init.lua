local plugins ={
  {
    paq = {'savq/paq-nvim', opt = true},
    setup = require('plugins.paq').setup
  }, {
    paq = {'windwp/nvim-autopairs', as = 'autopairs.nvim'},
    setup = require('nvim-autopairs').setup
  }, {
    paq = {'norcalli/nvim-colorizer.lua', as = 'colorizer.nvim'},
    setup = require('colorizer').setup
  }, {
    paq = {'tpope/vim-commentary', as = 'commentary.vim'}
  }, {
    paq = {'hrsh7th/nvim-compe', as = 'compe.nvim'},
    setup = require('plugins.compe').setup
  }, {
    paq = {'mfussenegger/nvim-dap', as = 'dap.nvim'},
    setup = require('plugins.dap').setup
  }, {
    paq = {'rcarriga/nvim-dap-ui', as = 'dap-ui.nvim'},
    setup = require('dapui').setup
  }, {
    paq = {'tommcdo/vim-exchange', as = 'exchange.vim'}
  }, {
    paq = {'junegunn/fzf.vim'},
    setup = require('plugins.fzf').setup
  }, {
    paq = {'phaazon/hop.nvim'},
    setup = require('plugins.hop').setup
  }, {
    paq = {'tommcdo/vim-lion', as = 'lion.vim'},
    setup = function()
      vim.g.lion_squeeze_spaces = 1
    end
  }, {
    paq = {'neovim/nvim-lspconfig', as = 'lspconfig.nvim'},
    setup = require('plugins.lspconfig').setup
  }, {
    paq = {'ishan9299/modus-theme-vim', as = 'modus-theme.nvim'},
    setup = require('plugins.modus-theme').setup
  }, {
    paq = {'kristijanhusak/orgmode.nvim'},
    setup = require('orgmode').setup
  }, {
    paq = {'tpope/vim-repeat', as = 'repeat.vim'}
  }, {
    paq = {'dstein64/nvim-scrollview', as = 'scrollview.nvim'}
  }, {
    paq = {'tpope/vim-surround', as = 'surround.vim'}
  }, {
    paq = {'nvim-treesitter/nvim-treesitter', as = 'tree-sitter.nvim', run = ':TSUpdate'}
  }, {
    paq = {'tpope/vim-unimpaired', as = 'unimpaired.vim'}
  }, {
    paq = {'tpope/vim-vinegar', as = 'vinegar.vim'},
    setup = require('plugins.vinegar').setup
  }, {
    paq = {'folke/which-key.nvim'},
    setup = require('plugins.which-key').setup
  }
}

local paqs = {}
for i,p in ipairs(plugins) do
  paqs[i] = p.paq
  if p.setup then p.setup() end
end

local function init_paq()
  vim.cmd [[packadd paq-nvim]]
  require 'paq-nvim' (paqs)
end

return {init_paq = init_paq}
