local utils = require('utils')

local function setup()
  vim.g.netrw_banner       = 0
  vim.g.netrw_browse_split = 4
  vim.g.netrw_dirhistmax   = 0
  vim.g.netrw_keepdir      = 1
  vim.g.netrw_list_hide    = [[\(^\|\s\s\)\zs\.\S\+]]
  vim.g.netrw_liststyle    = 3
  vim.g.netrw_winsize      = 25

  utils.nmap('<F9>', [[<Cmd>Lexplore<CR>]])
end

return {setup = setup}
