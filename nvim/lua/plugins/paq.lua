local utils = require('utils')

local function setup()
  local wk = utils.require('which-key')
  if next(wk) == nil then return end

  wk.register {
    ["<Leader>q"] = {
      name = "+paq",
      ['c'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq'.clean()<CR>]],     'Remove unlisted plugins'},
      ['i'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq'.install()<CR>]],   'Install listed plugins'},
      ['l'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq'.list()<CR>]],      'List installed plugins'},
      ['o'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq'.log_open()<CR>]],  'Open log'},
      ['u'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq'.update()<CR>]],    'Update all installed plugins'},
      ['x'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq'.log_clean()<CR>]], 'Clear log'}
    }
  }
end

return {setup = setup}
