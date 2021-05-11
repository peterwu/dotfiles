""""""""""""
" defaults "
""""""""""""
set number relativenumber cursorline lazyredraw showmatch hidden
set smartindent expandtab tabstop=2 shiftwidth=2 softtabstop=2
set smartcase ignorecase shortmess+=I mouse=a noshowmode
set nolist listchars=trail:·,tab:»·
set inccommand=nosplit path+=**
set termguicolors background=light

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
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

" terminal
" tnoremap <Esc> <C-\><C-n>
nnoremap <silent> <Leader>o :below 10sp term://$SHELL<CR>i

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

"""""""""""""""""""
" sneak
let g:sneak#label = 1

"""""""""""""""""""
" lightline
let g:lightline = {
      \   'colorscheme': 'solarized',
      \   'active': {
      \     'left': [ [ 'mode', 'paste' ],
      \               [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \   },
      \   'component_function': {
      \     'gitbranch': 'FugitiveHead'
      \   },
      \ }

"""""""""""""""""""
" fzf
nnoremap <Leader>f. :Files .<CR>
nnoremap <Leader>f` :Files /<CR>
nnoremap <Leader>f~ :Files $HOME<CR>
nnoremap <Leader>f% :Files %:p:h<CR>

nnoremap <Leader>f? :History<CR>
nnoremap <Leader>f: :History:<CR>
nnoremap <Leader>f/ :History/<CR>

nnoremap <Leader>fb :Buffers<CR>
nnoremap <Leader>fh :Helptags<CR>
nnoremap <Leader>fm :Maps<CR>
nnoremap <Leader>ft :Tags<CR>

" fugitive
nnoremap <Leader>gs :Git<CR>
nnoremap <Leader>gb :Git blame<CR>
nnoremap <Leader>gr :Gread<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gl :Gclog<CR>
nnoremap <Leader>gp :Git push<CR>

nnoremap <Leader>gd :Gvdiffsplit<CR>
nnoremap gdh        :diffget //2<CR>
nnoremap gdl        :diffget //3<CR>

"""""""""""""""""""
" lion
let g:lion_squeeze_spaces = 1

"""""""""""""""""""
" vinegar
nnoremap - k^

"""""""""""""""""""
" lspconfig
lua <<EOF
vim.cmd('packadd nvim-lspconfig')
local nvim_lsp = require'lspconfig'
nvim_lsp.clangd.setup{
  filetypes = { "c", "cpp"}
}
EOF

" use :setf + ctrl-d to see the list of filetypes
augroup lspconfig | autocmd!
  autocmd FileType c,cpp call s:lspconfig_mappings()
  autocmd FileType c,cpp setlocal omnifunc=v:lua.vim.lsp.omnifunc
  autocmd FileType c,cpp let b:vcm_tab_complete = "omni"
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

"""""""""""""""""""
" ide setup

" f5            : start debugging / continue
" ctrl-f5       : run without debugging
" shift-f5      : stop debugging
" ctrl-shift-f5 : restart debugging
" f7            : step into
" f8            : step over
" shift-f8      : step out




"""""""""""""""""""
" termdebug
let g:termdebug_wide = 1

""""""""""""""""""
" minpac plugins "
""""""""""""""""""
if exists('g:loaded_minpac')
  call minpac#init({'dir': split(&packpath, ",")[2]}) " -> $HOME.'/.local/share/nvim/site'
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  call minpac#add('jiangmiao/auto-pairs',          {'name': 'vim-auto-pairs'})
  call minpac#add('tpope/vim-commentary')
  call minpac#add('machakann/vim-highlightedyank', {'name': 'vim-highlighted-yank'})
  call minpac#add('tpope/vim-repeat')
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')
  call minpac#add('justinmk/vim-sneak')
  call minpac#add('tommcdo/vim-exchange')
  call minpac#add('tommcdo/vim-lion')

  call minpac#add('tpope/vim-fugitive')
  call minpac#add('tpope/vim-rhubarb')

  call minpac#add('tpope/vim-eunuch')
  call minpac#add('tpope/vim-vinegar')
  call minpac#add('mattn/emmet-vim',  {'name': 'vim-emmet'})
  call minpac#add('junegunn/fzf.vim', {'name': 'vim-fzf'})

  call minpac#add('sheerun/vim-polyglot')
  call minpac#add('chrisbra/colorizer',    {'name': 'vim-colorizer'})
  call minpac#add('itchyny/lightline.vim', {'name': 'vim-lightline'})

  call minpac#add('neovim/nvim-lspconfig')
  call minpac#add('ajh17/VimCompletesMe', {'name': 'vim-vcm'})
endif

" define user commands for updating/cleaning the plugins
command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()
command! PackStatus packadd minpac | source $MYVIMRC | call minpac#status()
