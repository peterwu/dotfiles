local utils = require('utils')

local function setup()
  local compe = utils.load('compe')
  if not compe then return end

  compe.setup {
    enabled          = true,
    autocomplete     = true,
    debug            = false,
    min_length       = 1,
    preselect        = 'enable',
    throttle_time    = 80,
    source_timeout   = 200,
    incomplete_delay = 400,
    max_abbr_width   = 100,
    max_kind_width   = 100,
    max_menu_width   = 100,
    documentation    = true,

    source = {
      path      = true,
      buffer    = true,
      calc      = true,
      nvim_lsp  = true,
      nvim_lua  = true,
      vsnip     = true,
      ultisnips = true
    }
  }

  local opts = {noremap=true, silent=true, expr=true}

  vim.api.nvim_set_keymap('i', '<Tab>',   [[pumvisible() ? '<C-N>' : '<Tab>']], opts)
  vim.api.nvim_set_keymap('i', '<S-Tab>', [[pumvisible() ? '<C-P>' : '<C-H>']], opts)
end

return {setup = setup}
