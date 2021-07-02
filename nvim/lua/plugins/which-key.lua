local function setup()
  require('which-key').setup {
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
