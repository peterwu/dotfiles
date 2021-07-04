local function require(module)
  local r, m = pcall(_G.require, module)
  return r and m or {}
end

return {require = require}
