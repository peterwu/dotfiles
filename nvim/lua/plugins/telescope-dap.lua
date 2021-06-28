local function setup()
  require('telescope').load_extension('dap')

  local opts = {noremap=true, silent=true}

  vim.api.nvim_set_keymap('n', '<Leader>fdd', [[<Cmd>Telescope dap commands<CR>]],         opts)
  vim.api.nvim_set_keymap('n', '<Leader>fdc', [[<Cmd>Telescope dap configurations<CR>]],   opts)
  vim.api.nvim_set_keymap('n', '<Leader>fdb', [[<Cmd>Telescope dap list_breakpoints<CR>]], opts)
  vim.api.nvim_set_keymap('n', '<Leader>fdv', [[<Cmd>Telescope dap variables<CR>]],        opts)
  vim.api.nvim_set_keymap('n', '<Leader>fdf', [[<Cmd>Telescope dap frames<CR>]],           opts)
end

return {setup = setup}
