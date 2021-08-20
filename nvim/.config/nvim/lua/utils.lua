local M = {}

M.require = function(module)
  local r, m = pcall(require, module)
  return r and m or {}
end

local map_modes = {'', 'n', 'v', 'x', 's', 'o', 'i', 'l', 'c', 't'}
for _, m in ipairs(map_modes) do
  M[m .. 'map'] = function(lhs, rhs, opts)
    local o = {noremap = true, silent = true}

    if opts and next(opts) ~= nil then
      for k, v in pairs(opts) do
        o[k] = v
      end
    end

    vim.api.nvim_set_keymap(m, lhs, rhs, o)
  end
end

return M
