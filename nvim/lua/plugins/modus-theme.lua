local function setup()
  vim.g.modus_cursorline_intense = 1
  vim.g.modus_green_strings      = 1
  vim.g.modus_termtrans_enable   = 1
  vim.g.modus_yellow_comments    = 1

  vim.cmd [[colorscheme modus-operandi]]
end


return {setup = setup}
