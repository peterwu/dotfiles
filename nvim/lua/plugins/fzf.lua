local function setup()
  vim.api.nvim_set_keymap('n', '<Leader>f.', [[<Cmd>Files .<CR>]],     {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>f`', [[<Cmd>Files /<CR>]],     {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>f~', [[<Cmd>Files $HOME<CR>]], {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>f%', [[<Cmd>Files %:p:h<CR>]], {noremap = true})

  vim.api.nvim_set_keymap('n', '<Leader>f?', [[<Cmd>History<CR>]],  {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>f:', [[<Cmd>History:<CR>]], {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>f/', [[<Cmd>History/<CR>]], {noremap = true})

  vim.api.nvim_set_keymap('n', '<Leader>fb', [[<Cmd>Buffers<CR>]],  {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>fh', [[<Cmd>Helptags<CR>]], {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>fm', [[<Cmd>Maps<CR>]],     {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>ft', [[<Cmd>Tags<CR>]],     {noremap = true})
end

return {setup = setup}
