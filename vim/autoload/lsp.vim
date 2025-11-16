vim9script

export def SetKeymaps(): void
    nnoremap <buffer> <Leader>gd <Cmd>LspGotoDefinition<CR>
    nnoremap <buffer> <Leader>gh <Cmd>LspGotoDeclaration<CR>
    nnoremap <buffer> <Leader>gi <Cmd>LspGotoImpl<CR>
    nnoremap <buffer> <Leader>go <Cmd>LspDocumentSymbol<CR>
    nnoremap <buffer> <Leader>gr <Cmd>LspShowReferences<CR>
    nnoremap <buffer> <Leader>gt <Cmd>LspTypeDefinition<CR>

    nnoremap <buffer> <Leader>gD <Cmd>LspPeekDefinition<CR>
    nnoremap <buffer> <Leader>gH <Cmd>LspPeekDeclaration<CR>
    nnoremap <buffer> <Leader>gI <Cmd>LspPeekImpl<CR>
    nnoremap <buffer> <Leader>gO <Cmd>LspSymbolSearch<CR>
    nnoremap <buffer> <Leader>gR <Cmd>LspPeekReferences<CR>
    nnoremap <buffer> <Leader>gT <Cmd>LspPeekTypeDefinition<CR>

    nnoremap <buffer> <Leader>gk <Cmd>LspIncomingCalls<CR>
    nnoremap <buffer> <Leader>gj <Cmd>LspOutgoingCalls<CR>

    nnoremap <buffer> <Leader>ga <Cmd>LspCodeAction<CR>
    nnoremap <buffer> <Leader>gf <Cmd>LspFormat<CR>
    nnoremap <buffer> <Leader>gx <Cmd>LspCodeLens<CR>
    nnoremap <buffer> <Leader>gy <Cmd>LspOutline<CR>
    nnoremap <buffer> <Leader>gz <Cmd>LspFold<CR>
    nnoremap <buffer> <Leader>g~ <Cmd>LspInlayHints toggle<CR>
    nnoremap <buffer> <Leader>g! <Cmd>LspDiagShow<CR>

    inoremap <buffer> <C-k> <Cmd>LspShowSignature<CR>
    nnoremap <buffer> K     <Cmd>LspHover<CR>
    nnoremap <buffer> <F2>  <Cmd>LspRename<CR>

    nnoremap <buffer> [d <Cmd>LspDiagPrev<CR>
    nnoremap <buffer> ]d <Cmd>LspDiagNext<CR>
    nnoremap <buffer> [D <Cmd>LspDiagFirst<CR>
    nnoremap <buffer> ]D <Cmd>LspDiagLast<CR>
enddef
