local function load(module)
  local r, m = pcall(require, module)
  return r and m or nil
end

return {load = load}
