local function setup()
  vim.api.nvim_set_keymap('n', '<Leader>qc', [[<Cmd>PackerClean<CR>]],    {noremap = true, silent = true})
  vim.api.nvim_set_keymap('n', '<Leader>qi', [[<Cmd>PackerInstsall<CR>]], {noremap = true, silent = true})
  vim.api.nvim_set_keymap('n', '<Leader>qs', [[<Cmd>PackerSync<CR>]],     {noremap = true, silent = true})
  vim.api.nvim_set_keymap('n', '<Leader>qu', [[<Cmd>PackerUpdate<CR>]],   {noremap = true, silent = true})
end

return { setup = setup }
