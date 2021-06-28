local function setup()
  vim.api.nvim_set_keymap('n', '<Leader>ff', [[<Cmd>Telescope find_files<CR>]],   {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>fo', [[<Cmd>Telescope oldfiles<CR>]],     {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>fx', [[<Cmd>Telescope file_browser<CR>]], {noremap = true})

  vim.api.nvim_set_keymap('n', '<Leader>f:', [[<Cmd>Telescope command_history<CR>]], {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>f/', [[<Cmd>Telescope search_history<CR>]],  {noremap = true})

  vim.api.nvim_set_keymap('n', '<Leader>fb', [[<Cmd>Telescope buffers<CR>]],   {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>fh', [[<Cmd>Telescope help_tags<CR>]], {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>fm', [[<Cmd>Telescope keymaps<CR>]],   {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>ft', [[<Cmd>Telescope tags<CR>]],      {noremap = true})
end

return {setup = setup}
