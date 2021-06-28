local function setup()
  require('hop').setup {keys = 'etovxqpdygfblzhckisuran'}

  vim.api.nvim_set_keymap('n', '<Leader>jj', [[<Cmd>HopChar1<CR>]], {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>jf', [[<Cmd>HopChar2<CR>]], {noremap = true})
  vim.api.nvim_set_keymap('n', '<Leader>jw', [[<Cmd>HopWord<CR>]],  {noremap = true})
end

return {setup = setup}
