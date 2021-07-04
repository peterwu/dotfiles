local function require(module)
  local r, m = pcall(_G.require, module)
  return r and m or {}
end


local function map_prep_opts(opts)
  local o = {noremap = true, silent = true}

  if opts and next(opts) ~= nil then
    for k,v in pairs(opts) do
      o[k] = v
    end
  end

  return o
end

local function map(lhs, rhs, opts)
  opts = map_prep_opts(opts)
  vim.api.nvim_set_keymap('', lhs, rhs, opts)
end

local function imap(lhs, rhs, opts)
  opts = map_prep_opts(opts)
  vim.api.nvim_set_keymap('i', lhs, rhs, opts)
end

local function nmap(lhs, rhs, opts)
  opts = map_prep_opts(opts)
  vim.api.nvim_set_keymap('n', lhs, rhs, opts)
end

local function omap(lhs, rhs, opts)
  opts = map_prep_opts(opts)
  vim.api.nvim_set_keymap('o', lhs, rhs, opts)
end

local function vmap(lhs, rhs, opts)
  opts = map_prep_opts(opts)
  vim.api.nvim_set_keymap('v', lhs, rhs, opts)
end

return {
  require = require,

  map  = map,
  imap = imap,
  nmap = nmap,
  omap = omap,
  vmap = vmap
}
