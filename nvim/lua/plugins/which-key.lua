local utils = require('utils')

local function setup()
  local wk = utils.load('which-key')
  if not wk then return end

  wk.setup {
    plugins = {
      presets = {
        operators    = false,
        motions      = false,
        text_objects = false
      }
    }
  }
end

return {setup = setup}
