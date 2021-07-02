local function setup()
  local wk = require('which-key')

  wk.register({
    ["<leader>q"] = {
      name = "+paq",
      c = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.clean()<CR>]],     'PaqClean'   },
      i = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.install()<CR>]],   'PaqInstall' },
      l = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.list()<CR>]],      'PaqList'    },
      o = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.log_open()<CR>]],  'PaqLogOpen' },
      u = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.update()<CR>]],    'PaqUpdate'  },
      x = {[[<Cmd>lua require'plugins'.init_paq();require'paq-nvim'.log_clean()<CR>]], 'PaqLogClean'}
    }
  })
end

return {setup = setup}
