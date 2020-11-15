set guioptions-=T guioptions-=m guioptions-=L guioptions-=r guioptions-=e
Guifont Iosevka Fusion:h12:cDEFAULT:qDEFAULT

let $MYGVIMRC = stdpath('config')."/ginit.vim"
nnoremap <silent> <Leader>egv :execute "edit   ".$MYGVIMRC<CR>
nnoremap <silent> <Leader>sgv :execute "source ".$MYGVIMRC<CR>

nnoremap <C-F1> :if &go=~#'m'<Bar>set go-=m<Bar>else<Bar>set go+=m<Bar>endif<CR>
nnoremap <C-F2> :if &go=~#'T'<Bar>set go-=T<Bar>else<Bar>set go+=T<Bar>endif<CR>
nnoremap <C-F3> :if &go=~#'r'<Bar>set go-=r<Bar>else<Bar>set go+=r<Bar>endif<CR>
