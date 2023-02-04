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

" disable arrow keys
noremap <Up>    <Nop>
noremap <Down>  <Nop>
noremap <Left>  <Nop>
noremap <Right> <Nop>

inoremap <Up>    <Nop>
inoremap <Down>  <Nop>
inoremap <Left>  <Nop>
inoremap <Right> <Nop>

" use arrow keys to navigate windows
nnoremap <Up>    <C-w><C-k>
nnoremap <Down>  <C-w><C-j>
nnoremap <Left>  <C-w><C-h>
nnoremap <Right> <C-w><C-l>

" swap j/k <-> gj/gk
noremap <expr> j (v:count ? 'j' : 'gj')
noremap <expr> k (v:count ? 'k' : 'gk')

" stolen from neovim defaults
nnoremap Y     yg_
nnoremap <C-L> <Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-L><CR>
inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>
xnoremap *     y/\V<C-R>"<CR>
xnoremap #     y?\V<C-R>"<CR>
nnoremap &     <Cmd>&&<CR>

" copy to clipboard
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
        const l:reg = '"'
        const l:sel_save = &selection
        let &selection = "inclusive"
        const l:cb_save  = &clipboard
        const l:reg_save = getreg(reg)
        const l:reg_type = getregtype(reg)

        if a:type ==# "char"
            silent! execute 'normal! v`[o`]"' .. l:reg .. 'y'
        elseif a:type ==# "line"
            silent! execute 'normal! `[V`]"' .. l:reg .. 'y'
        elseif a:type ==# "v" || a:type ==# "V" || a:type ==# "\<C-V>" || a:type ==# "block"
            let &selection = l:sel_save
            const l:ve = &virtualedit

            if !(a:0 && a:1)
                set virtualedit=none
            endif

            silent execute 'normal! gv"' .. l:reg .. 'y'
            let &virtualedit = l:ve
        elseif a:type =~ '^\d\+$'
            silent! execute 'normal! ^v' .. a:type .. '$h"' .. l:reg .. 'y'
            if mode() ==# 'v'
                normal! v
            endif
        else
            let &selection = l:sel_save
            let &clipboard = l:cb_save
        endif

        silent! call system('/usr/bin/xsel -i -b -l /dev/null', getreg(l:reg))

        call setreg(l:reg, l:reg_save, l:reg_type)
        let &selection = l:sel_save
        let &clipboard = l:cb_save
    endfunction
endif

" paste from clipboard
if has('clipboard')
    nnoremap <silent> <Leader>p "+p
    nnoremap <silent> <Leader>P "+P
    vnoremap <silent> <Leader>p "+p
    vnoremap <silent> <Leader>P "+P
elseif executable('/usr/bin/xsel')
    nnoremap <silent> <Leader>p <Cmd>silent! let @"=system('xsel -o -b -l /dev/null')<CR>""p
    nnoremap <silent> <Leader>P <Cmd>silent! let @"=system('xsel -o -b -l /dev/null')<CR>""P
    vnoremap <silent> <Leader>p <Cmd>silent! let @"=system('xsel -o -b -l /dev/null')<CR>""p
    vnoremap <silent> <Leader>P <Cmd>silent! let @"=system('xsel -o -b -l /dev/null')<CR>""P
endif

" force saving files that otherwise require sudoedit
command! Wsudo execute "silent! write !sudo tee % >/dev/null" <Bar> edit!
nnoremap <Leader>W <Cmd>Wsudo<CR>

" terminal
tnoremap <Esc> <C-\><C-N>
nnoremap <silent> <Leader>tt <Cmd>belowright terminal<CR>
nnoremap <silent> <Leader>tv <Cmd>belowright vertical terminal<CR>
nnoremap <silent> <Leader>tT <Cmd>botright terminal<CR>
nnoremap <silent> <Leader>tV <Cmd>botright vertical terminal<CR>
