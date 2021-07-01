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

local function highlight(group, fg, bg, style)
  local cmd = table.concat ({
    'highlight ' .. group,
    'guifg=' .. fg,
    'guibg=' .. bg,
    'gui=' .. style
  }, ' ')

  vim.cmd(cmd)
end

local function get_vi_mode()
  local mode_color = {
    n      = colors.magenta,
    i      = colors.green,
    ic     = colors.green,
    ix     = colors.green,
    v      = colors.cyan,
    [''] = colors.cyan,
    V      = colors.cyan,
    c      = colors.yellow,
    R      = colors.red,
    Rc     = colors.red,
    Rv     = colors.red,
    Rx     = colors.red,
    t      = colors.blue,
    ['!']  = colors.blue
  }

  local mode_alias = {
    n      = 'NORMAL',
    i      = 'INSERT',
    ic     = 'INSERT',
    ix     = 'INSERT',
    v      = 'VISUAL',
    [''] = 'V·BLOCK',
    V      = 'V·LINE',
    c      = 'COMMAND',
    R      = 'REPLACE',
    Rc     = 'REPLACE',
    Rv     = 'V·REPLACE',
    Rx     = 'REPLACE',
    t      = 'TERM',
    ['!']  = 'SHELL'
  }

  local mode_code = vim.api.nvim_get_mode().mode

  highlight('StatusViMode', mode_color[mode_code], colors.bg_main, 'bold')

  return mode_alias[mode_code]
end

local function get_git_branch()
  local git_color, git_cmd, git_branch, git_status
  local git_icon = ''
  local git_dir = vim.fn.expand('%:p:h:S')

  git_cmd = 'git -C ' .. git_dir .. ' branch --show-current 2> /dev/null'
  git_branch = vim.fn.system(git_cmd)
  git_branch = vim.fn.trim(git_branch)

  if string.len(git_branch) == 0 then
    highlight('StatusGitBranch', colors.fg_main, colors.bg_main, 'NONE')
    return ''
  end

  git_cmd = 'git -C ' .. git_dir .. ' status --porcelain'
  git_status = vim.fn.system(git_cmd)

  if string.len(git_status) == 0 then 
    git_color = colors.green_active
  else
    git_color = colors.red_active
  end

  highlight('StatusGitBranch', git_color, colors.bg_main, 'NONE')

  return git_icon .. ' ' .. git_branch
end

local function format_file_size(size)
  if size == 0 or size == -1 or size == -2 then
    return ''
  end

  local _1k = 1024
  local _1m = 1024 * _1k
  local _1g = 1024 * _1m
  local _1t = 1024 * _1g
  local _1p = 1024 * _1t
  local _1e = 1024 * _1p

  if size < _1k then
    size = string.format('%db', size)
  elseif size < _1m then
    size = string.format('%.1fk',size/_1k)
  elseif size < _1g then
    size = string.format('%.1fm',size/_1m)
  elseif size < _1t then
    size = string.format('%.1fg',size/_1g)
  elseif size < _1p then
    size = string.format('%.1ft',size/_1t)
  elseif size < _1e then
    size = string.format('%.1fp',size/_1p)
  else -- math.maxinteger = 2^63 - 1
    size = string.format('%.1fe',size/_1e)
  end

  return size
end

local function get_file_size()
  local file = vim.fn.expand('%:p')
  local size = 0

  if string.len(file) == 0 then -- it's a buffer
    size = vim.fn.wordcount().bytes
  else
    size = vim.fn.getfsize(file)
  end

  return format_file_size(size)
end

local function get_file_format()
  return vim.bo.fileformat:upper()
end

local function get_file_encode()
  local encode = vim.bo.fenc ~= '' and vim.bo.fenc or vim.o.enc
  return encode:upper()
end

local function set_highlights()
  highlight('StatusFileName',   colors.fg_active,      colors.bg_active, 'bold')
  highlight('StatusFileState',  colors.purple_intense, colors.bg_main,   'bold')
  highlight('StatusFileSize',   colors.fg_main,        colors.bg_main,   'NONE')
  highlight('StatusFileFormat', colors.fg_active,      colors.bg_active, 'NONE')
  highlight('StatusFileEncode', colors.fg_active,      colors.bg_active, 'NONE')
  highlight('StatusPercent',    colors.fg_main,        colors.bg_main,   'NONE')
  highlight('StatusBlank',      colors.fg_main,        colors.bg_main,   'NONE')
end

local function build_status_line()
  local focus = vim.g.statusline_winid == vim.fn.win_getid()

  if not focus then 
    return ' '
  else
    return table.concat {
      '%#StatusBlank#',
      ' ',
      '%#StatusViMode#',
      get_vi_mode(),
      '%#StatusBlank#',
      ' ',
      '%#StatusFileName#',
      ' ',
      '%<%F',
      ' ',
      '%#StatusFileState#',
      '%m%r%h%w%q',
      '%#StatusBlank#',
      ' ',
      '%#StatusGitBranch#',
      get_git_branch(),
      '%=',
      '%#StatusFileSize#',
      get_file_size(),
      '%#StatusBlank#',
      ' ',
      '%#StatusFileFormat#',
      ' ',
      get_file_format(),
      ' | ',
      '%#StatusFileEncode#',
      get_file_encode(),
      ' ',
      '%#StatusBlank#',
      ' ',
      '%#StatusPercent#',
      '%P',
      '%#StatusBlank#',
      ' '
    }
  end
end

vim.opt.statusline = [[%!v:lua.require'statusline'.statusline()]]

vim.cmd [[
augroup StatusLine | autocmd!
  autocmd ColorScheme * lua require('statusline').highlights()
augroup END
]]

return {statusline = build_status_line, highlights = set_highlights}
