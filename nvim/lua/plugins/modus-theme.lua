local colors = {
    bg_main                 = '#ffffff',
    fg_main                 = '#000000',
    bg_dim                  = '#f8f8f8',
    fg_dim                  = '#282828',
    bg_alt                  = '#f0f0f0',
    fg_alt                  = '#505050',
    bg_active               = '#d7d7d7',
    fg_active               = '#0a0a0a',
    bg_inactive             = '#efefef',
    fg_inactive             = '#404148',
    red                     = '#a60000',
    red_alt                 = '#972500',
    red_alt_other           = '#a0132f',
    red_faint               = '#7f1010',
    red_alt_faint           = '#702f00',
    red_alt_other_faint     = '#7f002f',
    green                   = '#005e00',
    green_alt               = '#315b00',
    green_alt_other         = '#145c33',
    green_faint             = '#104410',
    green_alt_faint         = '#30440f',
    green_alt_other_faint   = '#0f443f',
    yellow                  = '#813e00',
    yellow_alt              = '#70480f',
    yellow_alt_other        = '#863927',
    yellow_faint            = '#5f4400',
    yellow_alt_faint        = '#5d5000',
    yellow_alt_other_faint  = '#5e3a20',
    blue                    = '#0031a9',
    blue_alt                = '#2544bb',
    blue_alt_other          = '#0000c0',
    blue_faint              = '#003497',
    blue_alt_faint          = '#0f3d8c',
    blue_alt_other_faint    = '#001087',
    magenta                 = '#721045',
    magenta_alt             = '#8f0075',
    magenta_alt_other       = '#5317ac',
    magenta_faint           = '#752f50',
    magenta_alt_faint       = '#7b206f',
    magenta_alt_other_faint = '#55348e',
    cyan                    = '#00538b',
    cyan_alt                = '#30517f',
    cyan_alt_other          = '#005a5f',
    cyan_faint              = '#005077',
    cyan_alt_faint          = '#354f6f',
    cyan_alt_other_faint    = '#125458',
    red_intense             = '#b60000',
    orange_intense          = '#904200',
    green_intense           = '#006800',
    yellow_intense          = '#605b00',
    blue_intense            = '#1f1fce',
    magenta_intense         = '#a8007f',
    purple_intense          = '#7f10d0',
    cyan_intense            = '#005f88',
    red_active              = '#8a0000',
    green_active            = '#004c2e',
    yellow_active           = '#702d1f',
    blue_active             = '#0030b4',
    magenta_active          = '#5c2092',
    cyan_active             = '#003f8a'
}

local function setup()
    vim.g.modus_cursorline_intense = 1
    vim.g.modus_green_strings      = 1
    vim.g.modus_termtrans_enable   = 1
    vim.g.modus_yellow_comments    = 1

    xpcall(function()
        vim.cmd [[colorscheme modus-operandi]]
    end, function(err)
    vim.cmd [[colorscheme zellner]]
end)
end

return {setup = setup, colors = colors}
