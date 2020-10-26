set guioptions-=T guioptions-=m guioptions-=L guioptions-=r guioptions-=e
" set guifont=JetBrains\ Mono\ 12
" set guifont=JetBrainsMono\ NF\ 12
GuiFont JetBrains Mono 10

nnoremap <leader>egv :e $MYGVIMRC<cr>
nnoremap <leader>sgv :source $MYGVIMRC<cr>

nnoremap <c-f1> :if &go=~#'m'<bar>set go-=m<bar>else<bar>set go+=m<bar>endif<cr>
nnoremap <c-f2> :if &go=~#'t'<bar>set go-=t<bar>else<bar>set go+=t<bar>endif<cr>
nnoremap <c-f3> :if &go=~#'r'<bar>set go-=r<bar>else<bar>set go+=r<bar>endif<cr>
