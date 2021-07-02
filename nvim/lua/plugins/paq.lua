local function setup()
  local opts = {noremap = true, silent = true}

  vim.api.nvim_set_keymap('n', '<Leader>qc', [[<Cmd>PaqClean<CR>]],    opts)
  vim.api.nvim_set_keymap('n', '<Leader>qi', [[<Cmd>PaqInstall<CR>]],  opts)
  vim.api.nvim_set_keymap('n', '<Leader>ql', [[<Cmd>PaqList<CR>]],     opts)
  vim.api.nvim_set_keymap('n', '<Leader>qo', [[<Cmd>PaqLogOpen<CR>]],  opts)
  vim.api.nvim_set_keymap('n', '<Leader>qu', [[<Cmd>PaqUpdate<CR>]],   opts)
  vim.api.nvim_set_keymap('n', '<Leader>qx', [[<Cmd>PaqLogClean<CR>]], opts)
end

return {setup = setup}
