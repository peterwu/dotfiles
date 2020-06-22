set nocompatible encoding=utf-8 fileencoding=utf-8 nrformats-=octal
filetype plugin indent on 

set number cursorline relativenumber showcmd wildmenu lazyredraw showmatch ruler hidden
set autoindent smartindent smarttab expandtab tabstop=2 shiftwidth=2 softtabstop=2
set smartcase incsearch hlsearch ignorecase shortmess+=I title mouse=a

syntax enable
set termguicolors
set background=dark
packadd! vim-dracula
colorscheme dracula

let g:lightline = {
      \ 'colorscheme': 'dracula',
      \ }

" set leader keys
let g:mapleader=" "
let g:maplocalleader=","

" swap j/k <-> gj/gk
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" copy to clipboard
nnoremap Y y$
vnoremap <Leader>y  "+y
nnoremap <Leader>y  "+y
nnoremap <Leader>Y  "+y$
nnoremap <Leader>yy  "+yy

" paste from clipboard
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P

" key mappings
nnoremap <Leader>cd :cd %:p:h<CR>:pwd<CR>
nnoremap <Leader>ev :e $MYVIMRC<CR>
nnoremap <Leader>sv :source $MYVIMRC<CR>
nnoremap <Leader>pu :PackUpdate<CR>
nnoremap <Leader>pc :PackClean<CR>
nnoremap <Leader>ps :PackStatus<CR>

" plugin settings
"""""""""""""""""""""
" quick scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

"""""""""""""""""""
" ctrlp
nnoremap <Leader>F  :CtrlP<CR>
nnoremap <Leader>f  :CtrlPMRU<CR>
nnoremap <Leader>b  :CtrlPBuffer<CR>

let g:ctrlp_show_hidden = 1

"""""""""""""""""""""
" nerdtree 
let g:NERDTreeMinimalUI = 1
let g:NERDTreeHijackNetrw = 0
let g:NERDTreeWinSize = 31
let g:NERDTreeChDirMode = 2
let g:NERDTreeAutoDeleteBuffer = 1
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeCascadeOpenSingleChildDir = 1

map <F8> :NERDTreeToggle<CR>

"""""""""""""""""""""
" functions
" function s:SetCursorLine()
"   set cursorline
"   highlight CursorLine cterm=NONE 
" endfunction

" augroup set_cursorline
"   autocmd!
"   autocmd VimEnter * call s:SetCursorLine()
" augroup END

"""""""""""""""""""""
" minpac 
if exists('*minpac#init')
  call minpac#init({'dir': $HOME.'/.local/share/nvim/site'})
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  call minpac#add('jiangmiao/auto-pairs', {'name': 'vim-auto-pairs'})
  call minpac#add('tpope/vim-commentary')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('machakann/vim-highlightedyank', {'name': 'vim-highlighted-yank'})
  call minpac#add('unblevable/quick-scope', {'name': 'vim-quick-scope'})
  call minpac#add('tpope/vim-repeat')
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')
  call minpac#add('easymotion/vim-easymotion')

  call minpac#add('preservim/nerdtree', {'name': 'vim-nerdtree'})
  call minpac#add('mattn/emmet-vim', {'name': 'vim-emmet'})
  call minpac#add('ctrlpvim/ctrlp.vim', {'name': 'vim-ctrlp'})

  call minpac#add('sheerun/vim-polyglot')
  call minpac#add('chrisbra/Colorizer', {'name': 'vim-colorizer'})
  call minpac#add('dracula/vim', {'name': 'vim-dracula'})
  call minpac#add('itchyny/lightline.vim', {'name': 'vim-lightline'})
endif

" Define user commands for updating/cleaning the plugins.
" Each of them loads minpac, reloads .vimrc to register the
" information of plugins, then performs the task.
command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()
