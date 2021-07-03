local utils = require('utils')

local function setup()
  local telescope = utils.load('telescope')
  if not telescope then return end

  telescope.load_extension('dap')

  local wk = utils.load('which-key')
  if not wk then return end

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
