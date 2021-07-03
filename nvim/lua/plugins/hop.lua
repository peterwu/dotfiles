local utils = require('utils')

local function setup()
  local hop = utils.load('hop')
  if not hop then return end
  local wk = utils.load('which-key')
  if not wk then return end

  hop.setup {keys = 'etovxqpdygfblzhckisuran'}

  wk.register {
    ["<Leader>j"] = {
      name = "+jump",
      ['1'] = {[[<Cmd>HopChar1<CR>]],   'Jump to char 1'},
      ['J'] = {[[<Cmd>HopChar1BC<CR>]], 'Jump to char 1 before cursor'},
      ['j'] = {[[<Cmd>HopChar1AC<CR>]], 'Jump to char 1 after cursor'},

      ['2'] = {[[<Cmd>HopChar2<CR>]],   'Jump to char 2'},
      ['F'] = {[[<Cmd>HopChar2BC<CR>]], 'Jump to char 2 before cursor'},
      ['f'] = {[[<Cmd>HopChar2AC<CR>]], 'Jump to char 2 after cursor'},

      ['3'] = {[[<Cmd>HopWord<CR>]],   'Jump to word'},
      ['W'] = {[[<Cmd>HopWordBC<CR>]], 'Jump to word before cursor'},
      ['w'] = {[[<Cmd>HopWordAC<CR>]], 'Jump to word after cursor'},

      ['4'] = {[[<Cmd>HopLine<CR>]],   'Jump to line'},
      ['L'] = {[[<Cmd>HopLineBC<CR>]], 'Jump to line before cursor'},
      ['l'] = {[[<Cmd>HopLineAC<CR>]], 'Jump to line after cursor'},

      ['5'] = {[[<Cmd>HopLineStart<CR>]],   'Jump to line start'},
      ['S'] = {[[<Cmd>HopLineStartBC<CR>]], 'Jump to line start before cursor'},
      ['s'] = {[[<Cmd>HopLineStartAC<CR>]], 'Jump to line start after cursor'}
    }
  }
end

return {setup = setup}
