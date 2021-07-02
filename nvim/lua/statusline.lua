local colors = require('plugins.modus-theme').colors

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

  if git_branch:len() == 0 then
    highlight('StatusGitBranch', colors.fg_main, colors.bg_main, 'NONE')
    return ''
  end

  git_cmd = 'git -C ' .. git_dir .. ' status --porcelain'
  git_status = vim.fn.system(git_cmd)

  if git_status:len() == 0 then 
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

  local _1K = 1024
  local _1M = 1024 * _1K
  local _1G = 1024 * _1M
  local _1T = 1024 * _1G
  local _1P = 1024 * _1T
  local _1E = 1024 * _1P

  if size < _1K then
    size = string.format('%dB', size)
  elseif size < _1M then
    size = string.format('%.1fK',size/_1K)
  elseif size < _1G then
    size = string.format('%.1fM',size/_1M)
  elseif size < _1T then
    size = string.format('%.1fG',size/_1G)
  elseif size < _1P then
    size = string.format('%.1fT',size/_1T)
  elseif size < _1E then
    size = string.format('%.1fP',size/_1P)
  else -- math.maxinteger = 2^63 -1
    size = string.format('%.1fE',size/_1E)
  end

  return size
end

local function get_file_size()
  local file = vim.fn.expand('%:p')
  local size = 0

  if file:len() == 0 then -- it's a buffer
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
  autocmd ColorScheme,WinEnter,BufEnter,BufWinEnter * lua require('statusline').highlights()
  autocmd VimResized * redrawstatus
augroup END
]]

return {statusline = build_status_line, highlights = set_highlights}
