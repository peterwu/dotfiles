local function setup()
  require('which-key').register {
    ["<Leader>q"] = {
      name = "+paq",
      ['c'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.clean()<CR>]],     'Remove unlisted plugins'},
      ['i'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.install()<CR>]],   'Install listed plugins'},
      ['l'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.list()<CR>]],      'List installed plugins'},
      ['o'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.log_open()<CR>]],  'Open log'},
      ['u'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.update()<CR>]],    'Update all installed plugins'},
      ['x'] = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.log_clean()<CR>]], 'Clear log'}
    }
  }
end

return {setup = setup}
