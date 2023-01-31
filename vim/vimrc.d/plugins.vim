" ---------------
" plugin settings
" ---------------
" fzf
let g:fzf_layout = { 'down': '40%' }
let g:fzf_preview_window = []

nnoremap <Leader>ff <Cmd>Files<CR>
nnoremap <Leader>fo <Cmd>History<CR>
nnoremap <Leader>f: <Cmd>Hisotry:<CR>
nnoremap <Leader>f/ <Cmd>Hisotry/<CR>
nnoremap <Leader>fb <Cmd>Buffers<CR>
nnoremap <Leader>fh <Cmd>Helptags<CR>

" lion
let g:lion_squeeze_spaces = 1

" lsp
let s:lsp_servers = [{
            \   'filetype': [ 'c', 'cpp' ],
            \   'path': '/usr/bin/clangd',
            \   'args': [ '--background-index' ]
            \ }]

let s:lsp_opts = { 'autoHighlightDiags': v:true }

augroup Lsp | autocmd!
    autocmd VimEnter * call LspAddServer(s:lsp_servers)
    autocmd VimEnter * call LspOptionsSet(s:lsp_opts)
    autocmd VimEnter * call s:SetLspKeyBinds()
augroup END

function! s:SetLspKeyBinds() abort
    nmap <buffer> gd <Cmd>LspGotoDefinition<CR>
    nmap <buffer> gr <Cmd>LspShowReferences<CR>
    nmap <buffer> gi <Cmd>LspGotoImpl<CR>
    nmap <buffer> gt <Cmd>LspGotoTypeDef<CR>
    nmap <buffer> gR <Cmd>LspRename<CR>
    nmap <buffer> [g <Cmd>LspDiagPrev<CR>
    nmap <buffer> ]g <Cmd>LspDiagNext<CR>
    nmap <buffer> K  <Cmd>LspHover<CR>
endfunction

" netrw
let g:netrw_dirhistmax   = 0
let g:netrw_liststyle    = 3
let g:netrw_banner       = 0
let g:netrw_browse_split = 4
let g:netrw_winsize      = 29
let g:netrw_list_hide    = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_keepdir      = 0

nmap <silent> <F9> <Cmd>Lexplore<CR>

" remap cd to be global
augroup NetrwGroup | autocmd!
    autocmd filetype netrw call s:NetrwMapping()
augroup END

function! s:NetrwMapping()
    nmap <buffer> <silent> <nowait> <LocalLeader>cd <Cmd>execute "cd ".b:netrw_curdir<CR>:pwd<CR>
endfunction

" rainbow
let g:rainbow_active = 1

" sneak
let g:sneak#label = 1

map <Leader>s <Plug>Sneak_s
map <Leader>S <Plug>Sneak_S

map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T

" vinegar
noremap - k^

" termdebug
let g:termdebug_wide = 1

" --------------
" minpac plugins
" --------------
function! s:PackInit() abort
    packadd minpac

    call minpac#init()
    call minpac#add('k-takata/minpac',               {'type': 'opt'})

    call minpac#add('jiangmiao/auto-pairs')
    call minpac#add('tpope/vim-commentary',          {'name': 'commentary'})
    call minpac#add('tommcdo/vim-exchange',          {'name': 'exchange'})
    call minpac#add('machakann/vim-highlightedyank', {'name': 'highlighted-yank'})
    call minpac#add('tommcdo/vim-lion',              {'name': 'lion'})
    call minpac#add('tpope/vim-repeat',              {'name': 'repeat'})
    call minpac#add('justinmk/vim-sneak',            {'name': 'sneak'})
    call minpac#add('tpope/vim-surround',            {'name': 'surround'})
    call minpac#add('tpope/vim-unimpaired',          {'name': 'unimpaired'})

    call minpac#add('tpope/vim-eunuch',              {'name': 'eunuch'})
    call minpac#add('tpope/vim-fugitive',            {'name': 'fugitive'})
    call minpac#add('tpope/vim-vinegar',             {'name': 'vinegar'})

    call minpac#add('chrisbra/colorizer',            {'name': 'colorizer'})
    call minpac#add('luochen1990/rainbow')

    call minpac#add('junegunn/fzf.vim',              {'name': 'fzf'})

    call minpac#add('yegappan/lsp')
endfunction

" define user commands for updating/cleaning the plugins
command! PackUpdate source $MYVIMRC | call <SID>PackInit() | call minpac#update()
command! PackClean  source $MYVIMRC | call <SID>PackInit() | call minpac#clean()
command! PackStatus source $MYVIMRC | call <SID>PackInit() | call minpac#status()
