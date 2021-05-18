lua << EOF
-- defaults
local o = vim.o
local wo = vim.wo
local bo = vim.bo

o.background    = 'light'
o.hidden        = true
o.ignorecase    = true
o.inccommand    = 'nosplit'
o.lazyredraw    = true
o.mouse         = 'a'
o.path          = o.path .. '**'
o.shortmess     = o.shortmess .. 'I'
o.showmatch     = true
o.showmode      = false
o.smartcase     = true
o.termguicolors = true

wo.cursorline     = true
wo.list           = false
-- listchars      =trail:·,tab:»·
wo.number         = true
wo.relativenumber = true

-- key mappings
vim.g.mapleader      = " "
vim.g.maplocalleader = ","

vim.cmd [[

nnoremap <silent> <Leader>cd <Cmd>cd     %:p:h<CR>:pwd<CR>
nnoremap <silent> <Leader>ev <Cmd>edit   $MYVIMRC<CR>
nnoremap <silent> <Leader>sv <Cmd>source $MYVIMRC<CR>

nnoremap <silent> <Leader>pi <Cmd>PackerInstall<CR>
nnoremap <silent> <Leader>pu <Cmd>PackerUpdate<CR>
nnoremap <silent> <Leader>pc <Cmd>PackerClean<CR>
nnoremap <silent> <Leader>ps <Cmd>PackerSync<CR>

" disable arrow keys
noremap <Up>    <Nop>
noremap <Down>  <Nop>
noremap <Left>  <Nop>
noremap <Right> <Nop>

inoremap <Up>    <Nop>
inoremap <Down>  <Nop>
inoremap <Left>  <Nop>
inoremap <Right> <Nop>

" swap j/k <-> gj/gk
nnoremap <expr> j (v:count? 'j' : 'gj')
nnoremap <expr> k (v:count? 'k' : 'gk')

" copy to clipboard
nnoremap Y          y$
vnoremap <Leader>y  "+y
nnoremap <Leader>y  "+y
nnoremap <Leader>Y  "+y$
nnoremap <Leader>yy "+yy

" paste from clipboard
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P

" use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> <Cmd>nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

" terminal
" tnoremap <Esc> <C-\><C-n>
nnoremap <silent> <Leader>o <Cmd>below 10sp term://$SHELL<CR>i

" restore last known cursor position
" :help restore-cursor
autocmd BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

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

vim.cmd [[ nnoremap <silent> <F9> :Lexplore<CR> ]]
EOF

"""""""""""""""""""
" termdebug
lua << EOF
vim.g.termdebug_wide = 1

-- packer
vim.cmd ('packadd packer.nvim')

local packer = require('packer')
packer.startup(function()
  use {'wbthomason/packer.nvim', opt = true}

  use {'tpope/vim-sleuth', as = 'sleuth.vim'}

  use {'jiangmiao/auto-pairs',          as = 'auto-pairs.vim'}
  use {'tpope/vim-commentary',          as = 'commentary.vim'}
  use {'machakann/vim-highlightedyank', as = 'highlighted-yank.vim'}
  use {'tpope/vim-repeat',              as = 'repeat.vim'}
  use {'tpope/vim-surround',            as = 'surround.vim'}
  use {'tpope/vim-unimpaired',          as = 'unimpaired.vim'}
  use {'justinmk/vim-sneak',            as = 'sneak.vim', config = [[ vim.g['sneak#label'] = 1 ]]}
  use {'tommcdo/vim-exchange',          as = 'exchange.vim'}
  use {'tommcdo/vim-lion',              as = 'lion.vim', config = [[ vim.g.lion_squeeze_spaces = 1 ]]}

  use {'tpope/vim-fugitive', as = 'fugitive.vim', config = vim.cmd [[

  nnoremap <Leader>gs <Cmd>Git<CR>
  nnoremap <Leader>gb <Cmd>Git blame<CR>
  nnoremap <Leader>gr <Cmd>Gread<CR>
  nnoremap <Leader>gw <Cmd>Gwrite<CR>
  nnoremap <Leader>gl <Cmd>Gclog<CR>
  nnoremap <Leader>gp <Cmd>Git push<CR>

  nnoremap <Leader>gd <Cmd>Gvdiffsplit<CR>
  nnoremap gdh        <Cmd>diffget //2<CR>
  nnoremap gdl        <Cmd>diffget //3<CR>

  ]]}
  use {'tpope/vim-rhubarb',  as = 'rhubarb.vim'}

  use {'tpope/vim-eunuch',  as = 'eunuch.vim'}

  use {'tpope/vim-vinegar', as = 'vinegar.vim', config = vim.cmd [[
  nnoremap - k^
  ]]}

  use {'mattn/emmet-vim',   as = 'emmet.vim'}
  use {'junegunn/fzf.vim',  as = 'fzf.vim', config = vim.cmd [[

  nnoremap <Leader>f. <Cmd>Files .<CR>
  nnoremap <Leader>f` <Cmd>Files /<CR>
  nnoremap <Leader>f~ <Cmd>Files $HOME<CR>
  nnoremap <Leader>f% <Cmd>Files %:p:h<CR>

  nnoremap <Leader>f? <Cmd>History<CR>
  nnoremap <Leader>f: <Cmd>History:<CR>
  nnoremap <Leader>f/ <Cmd>History/<CR>

  nnoremap <Leader>fb <Cmd>Buffers<CR>
  nnoremap <Leader>fh <Cmd>Helptags<CR>
  nnoremap <Leader>fm <Cmd>Maps<CR>
  nnoremap <Leader>ft <Cmd>Tags<CR>

  ]]}

  use {'sheerun/vim-polyglot',      as = 'polyglot.vim'}
  use {'chrisbra/colorizer',        as = 'colorizer.vim'}

  use {'itchyny/lightline.vim',     as = 'lightline.vim', config = [[
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

  ]]}

use {'ishan9299/modus-theme-vim', as = 'modus-theme.nvim', config = [[

vim.g.modus_cursorline_intense = 1
vim.g.modus_green_strings      = 1
vim.g.modus_termtrans_enable   = 1
vim.g.modus_yellow_comments    = 1

vim.cmd('colorscheme modus-operandi') -- Light

]]}

  use {'neovim/nvim-lspconfig', as = 'lspconfig.nvim', config = [[
local nvim_lsp = require'lspconfig'

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

nvim_lsp.clangd.setup{
  filetypes = { "c", "cpp"};
  on_attach = on_attach
}

  ]]}

  use {'ajh17/VimCompletesMe',  as = 'vcm.vim'}

end)

EOF
