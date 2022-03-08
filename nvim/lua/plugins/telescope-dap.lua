local utils = require('utils')

local function setup()
  local telescope = utils.require('telescope')
  if next(telescope) == nil then return end

  local wk = utils.require('which-key')
  if next(wk) == nil then return end

  telescope.load_extension('dap')

  wk.register {
    ["<Leader>fd"] = {
      name = "+dap",
      ['d'] = {[[<Cmd>Telescope dap commands<CR>]],         'List commands'},
      ['c'] = {[[<Cmd>Telescope dap configurations<CR>]],   'List configurations'},
      ['b'] = {[[<Cmd>Telescope dap list_breakpoints<CR>]], 'List breakpoints'},
      ['v'] = {[[<Cmd>Telescope dap variables<CR>]],        'List variables'},
      ['f'] = {[[<Cmd>Telescope dap frames<CR>]],           'List frames'}
    }
  }
end
return {setup = setup}
