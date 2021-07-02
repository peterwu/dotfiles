local function setup()

  require('which-key').register {
    ["<Leader>f"] = {
      name = "+find",
      ['.'] = {[[<Cmd>Files .<CR>]],     'Find in current dir'},
      ['`'] = {[[<Cmd>Files /<CR>]],     'Find in root dir'},
      ['~'] = {[[<Cmd>Files $HOME<CR>]], 'Find in home dir'},
      ['%'] = {[[<Cmd>Files %:p:h<CR>]], 'Find in same dir'},
      
      ['?'] = {[[<Cmd>History<CR>]],  'Find recent files and open buffers'},
      [':'] = {[[<Cmd>History:<CR>]], 'Find recent commands'},
      ['/'] = {[[<Cmd>History/<CR>]], 'Find recent searches'},

      ['b'] = {[[<Cmd>Buffers<CR>]],  'Find buffers'},
      ['g'] = {[[<Cmd>GitFiles<CR>]], 'Find git status'},
      ['h'] = {[[<Cmd>Helptags<CR>]], 'Find help tags'},
      ['m'] = {[[<Cmd>Maps<CR>]],     'Find key maps'},
      ['t'] = {[[<Cmd>Tags<CR>]],     'Find tags'}
    }
  }

end

return {setup = setup}
