" --------
" defaults
" --------
source $VIMRUNTIME/defaults.vim

" ---------
" work dirs
" ---------
call mkdir($VIM_HOME .. "/cache", "p")
call mkdir($VIM_HOME .. "/swap",  "p")
call mkdir($VIM_HOME .. "/undo",  "p")
call mkdir($VIM_HOME .. "/view",  "p")

" -------
" options
" -------
set autoindent
set autoread
set background=light
set complete-=i
set completeopt=menu,menuone,noinsert,noselect
set cursorline
set directory=$VIM_HOME/swap
set encoding=utf-8
set expandtab
set fileformats=unix,dos,mac
set formatoptions=tcqj
set hidden
set history=1000
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
set scrolloff=1
set sessionoptions-=options
set shiftround
set shiftwidth=4
set shortmess+=Ic
set showmatch
set sidescroll=1
set sidescrolloff=2
set smartcase
set smartindent
set smarttab
set softtabstop=0
set splitbelow
set splitright
set tabstop=8
set termencoding=utf-8
set termguicolors
set timeoutlen=777
set ttimeoutlen=77
set undodir=$VIM_HOME/undo
set undofile
set viewdir=$VIM_HOME/view
set viewoptions-=options
set viminfo=%,<800,'10,/50,:100,h,f0,n$VIM_HOME/cache/viminfo
set virtualedit=block
set wildoptions=pum,tagfile

colorscheme lunaperche
