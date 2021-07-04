local utils = require('utils')

local function setup()
  local dap = utils.require('dap')
  if next(dap) == nil then return end

  dap.adapters.lldb = {
    type    = 'executable',
    command = '/usr/bin/lldb-vscode',
    name    = 'lldb'
  }

  dap.configurations.c = {
    {
      name = 'Launch',
      type = 'lldb',
      request = 'launch',
      program = function()
        return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
      end,
      cwd = '${workspaceFolder}',
      stopOnEntry = false,
      args = {},
      runInTerminal = true,
    },
  }

  dap.configurations.cpp = dap.configurations.c
  dap.configurations.rust = dap.configurations.cpp

  utils.nmap('<F5>',   [[<Cmd>lua require('dap').continue()<CR>]])
  utils.nmap('<F8>',   [[<Cmd>lua require('dap').step_over()<CR>]])
  utils.nmap('<F7>',   [[<Cmd>lua require('dap').into()<CR>]])
  utils.nmap('<S-F7>', [[<Cmd>lua require('dap').out()<CR>]])
  utils.nmap('<F6>',   [[<Cmd>lua require('dap').toggle_breakpoint()<CR>]])
  utils.nmap('<F10>',  [[<Cmd>lua require('dap').repl.open()<CR>]])
end

return {setup = setup}
