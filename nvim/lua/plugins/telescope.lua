local utils = require('utils')

local function setup()
  local wk = utils.require('which-key')
  if next(wk) == nil then return end

  wk.register {
    ["<Leader>f"] = {
      name = "+find",
      ['f'] = {[[<Cmd>Telescope find_files<CR>]],   'Find files'} ,
      ['o'] = {[[<Cmd>Telescope oldfiles<CR>]],     'Find old files'},
      ['x'] = {[[<Cmd>Telescope file_browser<CR>]], 'Launch file browser'},

      [':'] = {[[<Cmd>Telescope command_history<CR>]], 'Find recent commands'},
      ['/'] = {[[<Cmd>Telescope search_history<CR>]] , 'Find recent searches'},

      ['b'] = {[[<Cmd>Telescope buffers<CR>]],   'Find buffers'},
      ['h'] = {[[<Cmd>Telescope help_tags<CR>]], 'Find help tags'},
      ['m'] = {[[<Cmd>Telescope keymaps<CR>]],   'Find key maps'},
      ['t'] = {[[<Cmd>Telescope tags<CR>]],      'Find tags'}
    }
  }
end

return {setup = setup}
