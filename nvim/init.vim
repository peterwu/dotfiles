set nocompatible encoding=utf-8 fileencoding=utf-8 nrformats-=octal
filetype plugin indent on 
syntax enable

set number cursorline relativenumber showcmd wildmenu lazyredraw showmatch ruler hidden
set autoindent smartindent smarttab expandtab tabstop=2 shiftwidth=2 softtabstop=2
set smartcase incsearch hlsearch ignorecase shortmess+=I title mouse=a laststatus=2
set inccommand=nosplit
set path+=**

set termguicolors
set background=light
colorscheme zellner

""""""""""""""""
" key mappings "
""""""""""""""""
let g:mapleader      = " "
let g:maplocalleader = ","

nnoremap <leader>cd  :cd %:p:h<cr>:pwd<cr>
nnoremap <leader>ev  :e $MYVIMRC<cr>
nnoremap <leader>sv  :source $MYVIMRC<cr>
nnoremap <leader>pu  :PackUpdate<cr>
nnoremap <leader>pc  :PackClean<cr>
nnoremap <leader>ps  :PackStatus<cr>

" Disable Arrow keys in Normal mode
map <up>    <nop>
map <down>  <nop>
map <left>  <nop>
map <right> <nop>

" Disable Arrow keys in Insert mode
imap <up>    <nop>
imap <down>  <nop>
imap <left>  <nop>
imap <right> <nop>

" swap j/k <-> gj/gk
nnoremap j  gj
nnoremap k  gk
nnoremap gj j
nnoremap gk k

" copy to clipboard
nnoremap Y          y$
vnoremap <leader>y  "+y
nnoremap <leader>y  "+y
nnoremap <leader>Y  "+y$
nnoremap <leader>yy "+yy

" paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

" restore last known cursor position
" :help restore-cursor
autocmd BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

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
let g:netrw_keepdir      = 0

map <f9> :Lexplore<cr>

" quick scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

"""""""""""""""""""
" lightline
let g:lightline = { 'colorscheme': 'OldHope' }

"""""""""""""""""""
" nnn
let g:nnn#set_default_mappings = 0
let g:nnn#layout               = { 'window': { 'width': 0.9, 'height': 0.6, 'highlight': 'Debug' } }

nnoremap <leader>n :NnnPicker '%:p:h'<cr>

"""""""""""""""""""
" fzf
nnoremap <leader>. :Files .<cr>
nnoremap <leader>` :Files /<cr>
nnoremap <leader>~ :Files $HOME<cr>
nnoremap <leader>% :Files %:p:h<cr>

nnoremap <leader>? :History<cr>
nnoremap <leader>: :History:<cr>
nnoremap <leader>/ :History/<cr>

nnoremap <leader>b :Buffers<cr>
nnoremap <leader>h :Helptags<cr>
nnoremap <leader>m :Maps<cr>

"""""""""""""""""""
" lion 
let g:lion_squeeze_spaces = 1

"""""""""""""""""""
" easymotion
map <leader> <plug>(easymotion-prefix)

"""""""""""""""""""
" vinegar
noremap - k^

""""""""""""""""""
" minpac plugins "
""""""""""""""""""
if exists('*minpac#init')
  call minpac#init({'dir': $HOME.'/.local/share/nvim/site'})
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
  call minpac#add('mattn/emmet-vim',    {'name': 'vim-emmet'})
  call minpac#add('junegunn/fzf.vim',   {'name': 'vim-fzf'})
  call minpac#add('mcchrish/nnn.vim',   {'name': 'vim-nnn'})

  call minpac#add('sheerun/vim-polyglot')
  call minpac#add('chrisbra/colorizer',    {'name': 'vim-colorizer'})
  call minpac#add('itchyny/lightline.vim', {'name': 'vim-lightline'})
endif

" Define user commands for updating/cleaning the plugins.
" Each of them loads minpac, reloads .vimrc to register the
" information of plugins, then performs the task.
command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()
