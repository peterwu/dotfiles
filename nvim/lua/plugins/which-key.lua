local utils = require('utils')

local function setup()
  local wk = utils.require('which-key')
  if next(wk) == nil then return end

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
