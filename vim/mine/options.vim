" --------
" defaults
" --------
source $VIMRUNTIME/defaults.vim

" ---------
" work dirs
" ---------
if has('win32')
    let $VIM_HOME = $HOME .. '/vimfiles'
else
    let $VIM_HOME = $HOME .. '/.vim'
endif

call mkdir($VIM_HOME .. "/cache", "p")
call mkdir($VIM_HOME .. "/swap", "p")
call mkdir($VIM_HOME .. "/undo", "p")

" -------
" options
" -------
set background=light
set completeopt=menu,menuone,noinsert,noselect
set cursorline
set expandtab
set hidden
set hlsearch
set ignorecase
set laststatus=2
set lazyredraw
set listchars=trail:·,tab:»·
set mouse=a
set nolist
set noshowmode
set notitle
set number
set path+=**
set pumheight=7
set relativenumber
set sessionoptions=folds
set shiftwidth=4
set shortmess+=Ic
set showmatch
set smartcase
set smartindent
set softtabstop=0
set splitbelow
set splitright
set directory=$VIM_HOME/swap
set tabstop=8
set termguicolors
set timeoutlen=777
set undodir=$VIM_HOME/undo
set undofile
set viewoptions=cursor,folds
set viminfo=%,<800,'10,/50,:100,h,f0,n$VIM_HOME/cache/viminfo

colorscheme lunaperche
