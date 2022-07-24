local utils = require('utils')
local colors = utils.require('plugins.modus-theme').colors

-- :help mode()
local modes = {
    ['n']    = {alias = 'NORMAL',    color = colors.magenta},
    ['no']   = {alias = 'O·PENDING', color = colors.yellow},
    ['nov']  = {alias = 'O·PENDING', color = colors.yellow},
    ['noV']  = {alias = 'O·PENDING', color = colors.yellow},
    ['no'] = {alias = 'O·PENDING', color = colors.yellow},
    ['niI']  = {alias = 'NORMAL',    color = colors.magenta},
    ['niR']  = {alias = 'NORMAL',    color = colors.magenta},
    ['niV']  = {alias = 'NORMAL',    color = colors.magenta},
    ['v']    = {alias = 'VISUAL',    color = colors.cyan},
    ['V']    = {alias = 'V·LINE',    color = colors.cyan},
    ['']   = {alias = 'V·BLOCK',   color = colors.cyan},
    ['s']    = {alias = 'SELECT',    color = colors.blue},
    ['S']    = {alias = 'S·LINE',    color = colors.blue},
    ['']   = {alias = 'S·BLOCK',   color = colors.blue},
    ['i']    = {alias = 'INSERT',    color = colors.green},
    ['ic']   = {alias = 'INSERT',    color = colors.green},
    ['ix']   = {alias = 'INSERT',    color = colors.green},
    ['R']    = {alias = 'REPLACE',   color = colors.red},
    ['Rc']   = {alias = 'REPLACE',   color = colors.red},
    ['Rv']   = {alias = 'V·REPLACE', color = colors.red},
    ['Rx']   = {alias = 'REPLACE',   color = colors.red},
    ['c']    = {alias = 'COMMAND',   color = colors.yellow},
    ['cv']   = {alias = 'EX',        color = colors.magenta},
    ['ce']   = {alias = 'EX',        color = colors.magenta},
    ['r']    = {alias = 'REPLACE',   color = colors.red},
    ['rm']   = {alias = 'MORE',      color = colors.purpose_intense},
    ['r?']   = {alias = 'CONFIRM',   color = colors.purpose_intense},
    ['t']    = {alias = 'TERM',      color = colors.blue},
    ['!']    = {alias = 'SHELL',     color = colors.blue}
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

local function get_vim_mode()
    local mode = vim.api.nvim_get_mode().mode

    highlight('StatusVimMode', modes[mode].color, colors.bg_main, 'bold')

    return modes[mode].alias
end

local function get_git_branch()
    local git_color, git_branch, git_status, git_status_cmd
    local git_icon = ''
    local git_dir = vim.fn.expand('%:p:h:S')

    git_status_cmd = 'git -C ' .. git_dir .. ' status --branch --porcelain'

    git_status = vim.fn.system(git_status_cmd)
    if vim.api.nvim_eval('v:shell_error') == 0 then
        git_status = vim.fn.split(git_status, '\n')
        git_branch = git_status[1]:match("^##%s(%w*).*$")
    else
        highlight('StatusGitBranch', colors.fg_main, colors.bg_main, 'NONE')
        return ''
    end

    if #git_status > 1 then
        git_color = colors.red_active
    else
        git_color = colors.green_active
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
        return ''
    else
        return table.concat {
            '%#StatusBlank#',
            ' ',
            '%#StatusVimMode#',
            get_vim_mode(),
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

-- StatusLine
vim.api.nvim_create_augroup('StatusLine', {})

vim.api.nvim_create_autocmd(
   {'ColorScheme','WinEnter','BufEnter','BufWinEnter'},
   {
      group='StatusLine',
      pattern='*',
      command=[[lua require('statusline').highlights()]]
   }
)

vim.api.nvim_create_autocmd(
   {'VimResized'},
   {
      group='StatusLine',
      pattern='*',
      command='redrawstatus'
   }
)

return {statusline = build_status_line, highlights = set_highlights}
