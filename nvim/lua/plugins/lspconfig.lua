local utils = require('utils')

local function setup()
  local nvim_lsp = utils.require('lspconfig')
  if next(nvim_lsp) == nil then return end

  local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- mappings
    local opts = {noremap=true, silent=true}

    buf_set_keymap('n', 'gD',         [[<Cmd>lua vim.lsp.buf.declaration()<CR>]],                                opts)
    buf_set_keymap('n', 'gd',         [[<Cmd>lua vim.lsp.buf.definition()<CR>]],                                 opts)
    buf_set_keymap('n', 'K',          [[<Cmd>lua vim.lsp.buf.hover()<CR>]],                                      opts)
    buf_set_keymap('n', 'gi',         [[<Cmd>lua vim.lsp.buf.implementation()<CR>]],                             opts)
    buf_set_keymap('n', '<C-k>',      [[<Cmd>lua vim.lsp.buf.signature_help()<CR>]],                             opts)
    buf_set_keymap('n', '<Leader>wa', [[<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>]],                       opts)
    buf_set_keymap('n', '<Leader>wr', [[<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>]],                    opts)
    buf_set_keymap('n', '<Leader>wl', [[<Cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>]], opts)
    buf_set_keymap('n', '<Leader>D',  [[<Cmd>lua vim.lsp.buf.type_definition()<CR>]],                            opts)
    buf_set_keymap('n', '<Leader>rn', [[<cmd>lua vim.lsp.buf.rename()<CR>]],                                     opts)
    buf_set_keymap('n', 'gr',         [[<Cmd>lua vim.lsp.buf.references()<CR>]],                                 opts)
    buf_set_keymap('n', '<Leader>e',  [[<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>]],               opts)
    buf_set_keymap('n', '[d',         [[<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>]],                           opts)
    buf_set_keymap('n', ']d',         [[<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>]],                           opts)
    buf_set_keymap('n', '<Leader>q',  [[<Cmd>lua vim.lsp.diagnostic.set_loclist()<CR>]],                         opts)
    buf_set_keymap("n", '<Leader>f',  [[<cmd>lua vim.lsp.buf.formatting()<CR>]],                                 opts)
  end

  nvim_lsp.clangd.setup {
    filetypes = {'c', 'cpp'},
    on_attach = on_attach
  }
end

return {setup = setup}
