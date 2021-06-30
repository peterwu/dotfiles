local function setup()
  require('compe').setup {
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

  vim.api.nvim_set_keymap('i', '<Tab>',   [[pumvisible() ? '<C-n>' : '<Tab>']], opts)
  vim.api.nvim_set_keymap('i', '<S-Tab>', [[pumvisible() ? '<C-p>' : '<C-h>']], opts)
end

return {setup = setup}
