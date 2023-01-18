local utils = require('utils')

local function setup()
    local wk = utils.require('which-key')
    if next(wk) == nil then return end

    wk.register {
        ["<Leader>f"] = {
            name = "+find",
            ['f'] = {[[<Cmd>Files<CR>]],    'Find files'} ,
            ['o'] = {[[<Cmd>History<CR>]],  'Find old files'},

            [':'] = {[[<Cmd>Hisotry:<CR>]], 'Find recent commands'},
            ['/'] = {[[<Cmd>Hisotry/<CR>]], 'Find recent searches'},

            ['b'] = {[[<Cmd>Buffers<CR>]],  'Find buffers'},
            ['h'] = {[[<Cmd>Helptags<CR>]], 'Find help tags'},
        }
    }
end

return {setup = setup}
