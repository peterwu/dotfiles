local utils = require('utils')

local plugins ={
    {
        paq = {'savq/paq-nvim', as = 'paq.nvim', opt = true},
        setup = utils.require('plugins.paq').setup
    },
    {
        paq = {'windwp/nvim-autopairs', as = 'autopairs.nvim'},
        setup = utils.require('nvim-autopairs').setup
    },
    {
        paq = {'NvChad/nvim-colorizer.lua', as = 'colorizer.nvim'},
        setup = utils.require('colorizer').setup
    },
    {
        paq = {'tpope/vim-commentary', as = 'commentary.vim'}
    },
    {
        paq = {'tommcdo/vim-exchange', as = 'exchange.vim'}
    },
    {
        paq = {'junegunn/fzf.vim', as = 'fzf.vim'},
        setup = utils.require('plugins.fzf').setup
    },
    {
        paq = {'phaazon/hop.nvim'},
        setup = utils.require('plugins.hop').setup
    },
    {
        paq = {'tommcdo/vim-lion', as = 'lion.vim'},
        setup = function() vim.g.lion_squeeze_spaces = 1 end
    },
    {
        paq = {'neovim/nvim-lspconfig', as = 'lspconfig.nvim'},
        setup = utils.require('plugins.lspconfig').setup
    },
    {
        paq = {'ishan9299/modus-theme-vim', as = 'modus-theme.nvim'},
        setup = utils.require('plugins.modus-theme').setup
    },
    {
        paq = {'tpope/vim-repeat', as = 'repeat.vim'}
    },
    {
        paq = {'dstein64/nvim-scrollview', as = 'scrollview.nvim'}
    },
    {
        paq = {'kylechui/nvim-surround', as = 'surround.nvim'},
        setup = utils.require('nvim-surround').setup
    },
    {
        paq = {'nvim-treesitter/nvim-treesitter', as = 'tree-sitter.nvim', run = function() vim.cmd('TSUpdate') end}
    },
    {
        paq = {'tpope/vim-unimpaired', as = 'unimpaired.vim'}
    },
    {
        paq = {'tpope/vim-vinegar', as = 'vinegar.vim'},
        setup = utils.require('plugins.vinegar').setup
    },
    {
        paq = {'folke/which-key.nvim'},
        setup = utils.require('plugins.which-key').setup
    }
}

local paqs = {}
for i, p in ipairs(plugins) do
    paqs[i] = p.paq
    pcall(p.setup)
end

local function init_paq()
    vim.cmd 'packadd paq.nvim'
    utils.require'paq'(paqs)
end

return {init_paq = init_paq}
