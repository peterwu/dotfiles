local function setup()
  local opts = {noremap = true, silent = true}

  vim.api.nvim_set_keymap('n', '<Leader>qc', [[<Cmd>PackerClean<CR>]],    opts)
  vim.api.nvim_set_keymap('n', '<Leader>qi', [[<Cmd>PackerInstsall<CR>]], opts)
  vim.api.nvim_set_keymap('n', '<Leader>qs', [[<Cmd>PackerSync<CR>]],     opts)
  vim.api.nvim_set_keymap('n', '<Leader>qu', [[<Cmd>PackerUpdate<CR>]],   opts)
end

return { setup = setup }
