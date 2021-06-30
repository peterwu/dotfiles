local function setup()
  local dap = require('dap')
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

  local opts = {noremap=true, silent=true}

  vim.api.nvim_set_keymap('n', '<F5>',   [[<Cmd>lua require('dap').continue()<CR>]],          opts)
  vim.api.nvim_set_keymap('n', '<F8>',   [[<Cmd>lua require('dap').step_over()<CR>]],         opts)
  vim.api.nvim_set_keymap('n', '<F7>',   [[<Cmd>lua require('dap').into()<CR>]],              opts)
  vim.api.nvim_set_keymap('n', '<S-F7>', [[<Cmd>lua require('dap').out()<CR>]],               opts)
  vim.api.nvim_set_keymap('n', '<F6>',   [[<Cmd>lua require('dap').toggle_breakpoint()<CR>]], opts)
  vim.api.nvim_set_keymap('n', '<F10>',  [[<Cmd>lua require('dap').repl.open()<CR>]],         opts)
end

return {setup = setup}
