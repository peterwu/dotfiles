" ------------
" key mappings
" ------------
" leaders
let g:mapleader      = " "
let g:maplocalleader = ","

" vimrc
nnoremap <silent> <Leader>cd <Cmd>cd     %:p:h<CR><Cmd>pwd<CR>
nnoremap <silent> <Leader>ev <Cmd>edit   $MYVIMRC<CR>
nnoremap <silent> <Leader>sv <Cmd>source $MYVIMRC<CR>

" minpac
nnoremap <silent> <Leader>qu <Cmd>PackUpdate<CR>
nnoremap <silent> <Leader>qc <Cmd>PackClean<CR>
nnoremap <silent> <Leader>qs <Cmd>PackStatus<CR>

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
nnoremap <expr> j (v:count ? 'j' : 'gj')
nnoremap <expr> k (v:count ? 'k' : 'gk')
vnoremap <expr> j (v:count ? 'j' : 'gj')
vnoremap <expr> k (v:count ? 'k' : 'gk')

" copy to clipboard
nnoremap Y yg_

if has('clipboard')
    vnoremap <silent> <Leader>y  "+y
    nnoremap <silent> <Leader>y  "+y
    nnoremap <silent> <Leader>Y  "+yg_
    nnoremap <silent> <Leader>yy "+y_
elseif executable('/usr/bin/xsel')
    vnoremap <silent> <Leader>y  <Cmd>set opfunc=<SID>CopyToClipboard<CR>g@
    nnoremap <silent> <Leader>y  <Cmd>set opfunc=<SID>CopyToClipboard<CR>g@
    nnoremap <silent> <Leader>Y  <Cmd>set opfunc=<SID>CopyToClipboard<CR>g@g_
    nnoremap <silent> <Leader>yy <Cmd>set opfunc=<SID>CopyToClipboard<CR>g@_

    function! s:CopyToClipboard(type, ...) abort
        let reg = '"'
        let sel_save = &selection
        let &selection = "inclusive"
        let cb_save  = &clipboard
        let reg_save = getreg(reg)
        let reg_type = getregtype(reg)
        if a:type ==# "char"
            silent execute 'normal! v`[o`]"'.reg.'y'
        elseif a:type ==# "line"
            silent execute 'normal! `[V`]"'.reg.'y'
        elseif a:type ==# "v" || a:type ==# "V" || a:type ==# "\<C-V>" || a:type ==# "block"
            let &selection = sel_save
            let ve = &virtualedit
            if !(a:0 && a:1)
                set virtualedit=none
            endif
            silent execute 'normal! gv"'.reg.'y'
            let &virtualedit = ve
        elseif a:type =~ '^\d\+$'
            silent execute 'normal! ^v'.a:type.'$h"'.reg.'y'
            if mode() ==# 'v'
                normal! v
            endif
        else
            let &selection = sel_save
            let &clipboard = cb_save
        endif

        call system('/usr/bin/xsel -i -b -l /dev/null', getreg(reg))

        call setreg(reg, reg_save, reg_type)
        let &selection = sel_save
        let &clipboard = cb_save
    endfunction
endif

" paste from clipboard
if has('clipboard')
    nnoremap <silent> <Leader>p "+p
    nnoremap <silent> <Leader>P "+P
    vnoremap <silent> <Leader>p "+p
    vnoremap <silent> <Leader>P "+P
elseif executable('/usr/bin/xsel')
    nnoremap <silent> <Leader>p <Cmd>let @"=system('xsel -o -b -l /dev/null')<CR>""p
    nnoremap <silent> <Leader>P <Cmd>let @"=system('xsel -o -b -l /dev/null')<CR>""P
    vnoremap <silent> <Leader>p <Cmd>let @"=system('xsel -o -b -l /dev/null')<CR>""p
    vnoremap <silent> <Leader>P <Cmd>let @"=system('xsel -o -b -l /dev/null')<CR>""P
endif

" use <C-L> to clear the highlighting of :set hlsearch
if maparg('<C-L>', 'n') ==# ''
    nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

" force saving files that otherwise require sudoedit
command! Wsudo execute "silent! write !sudo tee % >/dev/null" <Bar> edit!

" terminal
tnoremap <Esc> <C-\><C-N>
nnoremap <silent> <Leader>tt <Cmd>belowright terminal<CR>
nnoremap <silent> <Leader>tv <Cmd>belowright vertical terminal<CR>
nnoremap <silent> <Leader>tT <Cmd>botright terminal<CR>
nnoremap <silent> <Leader>tV <Cmd>botright vertical terminal<CR>
