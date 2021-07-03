local function setup()
  vim.g.netrw_banner       = 0
  vim.g.netrw_browse_split = 4
  vim.g.netrw_dirhistmax   = 0
  vim.g.netrw_keepdir      = 1
  vim.g.netrw_list_hide    = [[\(^\|\s\s\)\zs\.\S\+]]
  vim.g.netrw_liststyle    = 3
  vim.g.netrw_winsize      = 25

  vim.api.nvim_set_keymap('n', '<F9>', [[<Cmd>Lexplore<CR>]], {noremap = true, silent = true})
end

return {setup = setup}
