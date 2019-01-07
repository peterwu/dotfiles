set nocompatible encoding=utf-8 fileencoding=utf-8 nrformats-=octal
filetype plugin indent on 

set number relativenumber cursorline showcmd wildmenu lazyredraw showmatch ruler hidden
set guioptions-=T guioptions-=m guioptions-=L guioptions-=r 
set autoindent smartindent smarttab expandtab tabstop=2 shiftwidth=2 softtabstop=2
set smartcase incsearch hlsearch ignorecase shortmess+=I
set mouse=a

syntax enable
set termguicolors
set background=dark

colorscheme dracula

let g:airline_theme='dracula'
let g:airline_powerline_fonts = 1
let g:airline_extensions = ['branch', 'tabline', 'ale']
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

let g:prettier#config#bracket_spacing = 'true'
let g:prettier#config#single_quote = 'false'
let g:prettier#config#jsx_bracket_same_line = 'false'
let g:prettier#config#trailing_comma = 'none'
let g:prettier#config#parser = 'babylon'
let g:prettier#autoformat = 0

augroup vim-prettier
  autocmd!
  autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync
augroup END

" key mappings
nnoremap Y y$
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" minpac 
if exists('*minpac#init')
  call minpac#init({'dir': split(&packpath, ",")[2]})
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  call minpac#add('jiangmiao/auto-pairs', {'name': 'vim-auto-pairs'})
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')
  call minpac#add('machakann/vim-highlightedyank', {'name': 'vim-highlighted-yank'})
  call minpac#add('tpope/vim-repeat')
  call minpac#add('tpope/vim-commentary')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('unblevable/quick-scope', {'name': 'vim-quick-scope'})
  call minpac#add('junegunn/fzf.vim', {'name': 'vim-fzf'})
  call minpac#add('dracula/vim', {'name': 'vim-dracula'})
  call minpac#add('vim-airline/vim-airline')
  call minpac#add('w0rp/ale', {'name': 'vim-ale'})
  call minpac#add('prettier/vim-prettier', {'do': {-> system('npm install')}})
endif

" Define user commands for updating/cleaning the plugins.
" Each of them loads minpac, reloads .vimrc to register the
" information of plugins, then performs the task.
command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()
