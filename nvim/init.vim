""""""""""""
" defaults "
""""""""""""
set number relativenumber cursorline lazyredraw showmatch hidden
set smartindent expandtab tabstop=2 shiftwidth=2 softtabstop=2
set smartcase ignorecase shortmess+=I mouse=a noshowmode
set nolist listchars=trail:·,tab:»·
set inccommand=nosplit path+=**
set termguicolors
set omnifunc=syntaxcomplete#Complete

" colors
packadd! vim-dracula
colorscheme dracula
highlight SpecialKey ctermfg=60 guifg=#ffffa5
highlight Whitespace ctermfg=60 guifg=#ffffa5

""""""""""""""""
" key mappings "
""""""""""""""""
let g:mapleader      = " "
let g:maplocalleader = ","

nnoremap <silent> <Leader>cd :cd     %:p:h<CR>:pwd<CR>
nnoremap <silent> <Leader>ev :edit   $MYVIMRC<CR>
nnoremap <silent> <Leader>sv :source $MYVIMRC<CR>

nnoremap <silent> <Leader>pu :PackUpdate<CR>
nnoremap <silent> <Leader>pc :PackClean<CR>
nnoremap <silent> <Leader>ps :PackStatus<CR>

" disable arrow keys in normal mode
map <Up>    <Nop>
map <Down>  <Nop>
map <Left>  <Nop>
map <Right> <Nop>

" disable arrow keys in insert mode
imap <Up>    <Nop>
imap <Down>  <Nop>
imap <Left>  <Nop>
imap <Right> <Nop>

" swap j/k <-> gj/gk
noremap <expr> j (v:count? 'j' : 'gj')
noremap <expr> k (v:count? 'k' : 'gk')

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

" terminal
" tnoremap <Esc> <C-\><C-n>
nnoremap <Leader>o :below 10sp term://$SHELL<CR>i

" restore last known cursor position
" :help restore-cursor
autocmd BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

" set list when in insert mode
autocmd InsertEnter,InsertLeave * set list!

"""""""""""""""""""
" plugin settings "
"""""""""""""""""""
" netrw
let g:netrw_dirhistmax   = 0
let g:netrw_liststyle    = 3
let g:netrw_banner       = 0
let g:netrw_browse_split = 4
let g:netrw_winsize      = 25
let g:netrw_list_hide    = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_keepdir      = 1

nmap <silent> <F9> :Lexplore<CR>

" remap cd to be global
augroup NetrwGroup | autocmd!
  autocmd filetype netrw call s:NetrwMapping()
augroup END

function! s:NetrwMapping()
  nmap <buffer> <silent> <nowait> <LocalLeader>cd  :execute "cd ".b:netrw_curdir<CR>:pwd<CR>
endfunction

""""""""""""""""""
" quick scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

"""""""""""""""""""
" lightline
let g:lightline = {
      \   'colorscheme': 'dracula',
      \   'active': {
      \     'left': [ [ 'mode', 'paste' ],
      \               [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \   },
      \   'component_function': {
      \     'gitbranch': 'FugitiveHead'
      \   },
      \ }

"""""""""""""""""""
" nnn
let g:nnn#statusline           = 0
let g:nnn#replace_netrw        = 0
let g:nnn#set_default_mappings = 0
let g:nnn#layout               = { 'window': { 'width': 0.9, 'height': 0.6, 'highlight': 'Debug' } }

nnoremap <Leader>n :NnnPicker %:p:h<CR>

"""""""""""""""""""
" fzf
nnoremap <Leader>. :Files .<CR>
nnoremap <Leader>` :Files /<CR>
nnoremap <Leader>~ :Files $HOME<CR>
nnoremap <Leader>% :Files %:p:h<CR>

nnoremap <Leader>? :History<CR>
nnoremap <Leader>: :History:<CR>
nnoremap <Leader>/ :History/<CR>

nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :Helptags<CR>
nnoremap <Leader>m :Maps<CR>
nnoremap <Leader>t :Tags<CR>

"""""""""""""""""""
" lion
let g:lion_squeeze_spaces = 1

"""""""""""""""""""
" easymotion
map <Leader><Leader>. <Plug>(easymotion-repeat)

"""""""""""""""""""
" vinegar
noremap - k^

"""""""""""""""""""
" lspconfig
lua <<EOF
  vim.cmd('packadd nvim-lspconfig')
  local nvim_lsp = require'nvim_lsp'
  nvim_lsp.clangd.setup{
        filetypes = { "c", "cpp", "objc", "objcpp" }
  }
EOF

" use :setf + ctrl-d to see the list of filetypes
augroup lspconfig | autocmd!
  autocmd FileType c,cpp,objc,objcpp call s:lspconfig_mappings()
augroup END

function! s:lspconfig_mappings()
  nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
  nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
  nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
  nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
  nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
  nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
  nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
  nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
  nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
endfunction

""""""""""""""""""
" minpac plugins "
""""""""""""""""""
if exists('g:loaded_minpac')
  call minpac#init({'dir': split(&packpath, ",")[2]}) " -> $HOME.'/.local/share/nvim/site'
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  call minpac#add('jiangmiao/auto-pairs',          {'name': 'vim-auto-pairs'})
  call minpac#add('tpope/vim-commentary')
  call minpac#add('machakann/vim-highlightedyank', {'name': 'vim-highlighted-yank'})
  call minpac#add('unblevable/quick-scope',        {'name': 'vim-quick-scope'})
  call minpac#add('tpope/vim-repeat')
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')
  call minpac#add('easymotion/vim-easymotion')
  call minpac#add('tommcdo/vim-exchange')
  call minpac#add('tommcdo/vim-lion')

  call minpac#add('tpope/vim-fugitive')
  call minpac#add('tpope/vim-eunuch')
  call minpac#add('tpope/vim-vinegar')
  call minpac#add('mattn/emmet-vim',  {'name': 'vim-emmet'})
  call minpac#add('junegunn/fzf.vim', {'name': 'vim-fzf'})
  call minpac#add('mcchrish/nnn.vim', {'name': 'vim-nnn'})

  call minpac#add('dracula/vim',           {'name': 'vim-dracula'})
  call minpac#add('sheerun/vim-polyglot')
  call minpac#add('chrisbra/colorizer',    {'name': 'vim-colorizer'})
  call minpac#add('itchyny/lightline.vim', {'name': 'vim-lightline'})

  call minpac#add('neovim/nvim-lspconfig')
  call minpac#add('ajh17/VimCompletesMe', {'name': 'vim-vcm'})
endif

" Define user commands for updating/cleaning the plugins.
" Each of them loads minpac, reloads .vimrc to register the
" information of plugins, then performs the task.
command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()
