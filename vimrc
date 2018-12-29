set nocompatible encoding=utf-8 fileencoding=utf-8 nrformats-=octal
filetype plugin indent on 

set number relativenumber cursorline showcmd wildmenu lazyredraw showmatch ruler hidden
set guioptions-=T guioptions-=m guioptions-=L guioptions-=r 
set autoindent smartindent smarttab expandtab tabstop=2 shiftwidth=2 softtabstop=2
set smartcase incsearch hlsearch ignorecase shortmess+=I
set mouse=a

if has('win32') "Windows
  set guifont=Fira\ Code\ Retina:h12:cANSI
  set viminfo=%,<800,'10,/50,:100,h,f0,n~/vimfiles/cache/.viminfo
else
  set guifont=Fira\ Code\ Retina\ 12
  " set guifont=Inconsolata\ 16
  set viminfo=%,<800,'10,/50,:100,h,f0,n~/.vim/.viminfo
endif

syntax enable
set termguicolors
set background=dark

colorscheme dracula
let g:airline_theme='dracula'

" let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#ale#enabled = 1

let g:ctrlp_custom_ignore = {
      \ 'dir':  '\.git$\|node_modules\|log\|tmp$',
      \ 'file': '\.so$\|\.dat$|\.DS_Store$'
      \ }

let g:prettier#config#bracket_spacing = 'true'
let g:prettier#config#single_quote = 'false'
let g:prettier#config#jsx_bracket_same_line = 'false'
let g:prettier#config#trailing_comma = 'none'
let g:prettier#config#parser = 'babylon'
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync

" key mappings
nnoremap Y y$
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>
nnoremap <F3> :FZF<CR>

" augroup AutoSaveFolds
"   autocmd!
"   autocmd BufWinLeave * mkview
"   autocmd BufWinEnter * silent! loadview
" augroup END

" minpac 
if exists('*minpac#init')
  call minpac#init()
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  call minpac#add('jiangmiao/auto-pairs', {'name': 'vim-auto-pairs'})
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')
  call minpac#add('machakann/vim-highlightedyank')
  call minpac#add('tpope/vim-repeat')
  call minpac#add('tpope/vim-commentary')
  call minpac#add('junegunn/fzf.vim', {'name': 'vim-fzf'})
  call minpac#add('dracula/vim', {'name': 'vim-dracula'})
  call minpac#add('vim-airline/vim-airline')
  call minpac#add('mattn/emmet-vim', { 'name': 'vim-emmet' })
  call minpac#add('pangloss/vim-javascript')
  call minpac#add('posva/vim-vue')
  call minpac#add('jparise/vim-graphql')
  call minpac#add('w0rp/ale', {'name': 'vim-ale'})
  call minpac#add('prettier/vim-prettier', {'do': {-> system('npm install')}})
endif

" Define user commands for updating/cleaning the plugins.
" Each of them loads minpac, reloads .vimrc to register the
" information of plugins, then performs the task.
command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()
