" -----------
" Status Line
" -----------
const s:colors = {
            \    'bg_main'                 : '#ffffff' ,
            \    'fg_main'                 : '#000000' ,
            \    'bg_dim'                  : '#f8f8f8' ,
            \    'fg_dim'                  : '#282828' ,
            \    'bg_alt'                  : '#f0f0f0' ,
            \    'fg_alt'                  : '#505050' ,
            \    'bg_active'               : '#d7d7d7' ,
            \    'fg_active'               : '#0a0a0a' ,
            \    'bg_inactive'             : '#efefef' ,
            \    'fg_inactive'             : '#404148' ,
            \    'red'                     : '#a60000' ,
            \    'red_alt'                 : '#972500' ,
            \    'red_alt_other'           : '#a0132f' ,
            \    'red_faint'               : '#7f1010' ,
            \    'red_alt_faint'           : '#702f00' ,
            \    'red_alt_other_faint'     : '#7f002f' ,
            \    'green'                   : '#005e00' ,
            \    'green_alt'               : '#315b00' ,
            \    'green_alt_other'         : '#145c33' ,
            \    'green_faint'             : '#104410' ,
            \    'green_alt_faint'         : '#30440f' ,
            \    'green_alt_other_faint'   : '#0f443f' ,
            \    'yellow'                  : '#813e00' ,
            \    'yellow_alt'              : '#70480f' ,
            \    'yellow_alt_other'        : '#863927' ,
            \    'yellow_faint'            : '#5f4400' ,
            \    'yellow_alt_faint'        : '#5d5000' ,
            \    'yellow_alt_other_faint'  : '#5e3a20' ,
            \    'blue'                    : '#0031a9' ,
            \    'blue_alt'                : '#2544bb' ,
            \    'blue_alt_other'          : '#0000c0' ,
            \    'blue_faint'              : '#003497' ,
            \    'blue_alt_faint'          : '#0f3d8c' ,
            \    'blue_alt_other_faint'    : '#001087' ,
            \    'magenta'                 : '#721045' ,
            \    'magenta_alt'             : '#8f0075' ,
            \    'magenta_alt_other'       : '#5317ac' ,
            \    'magenta_faint'           : '#752f50' ,
            \    'magenta_alt_faint'       : '#7b206f' ,
            \    'magenta_alt_other_faint' : '#55348e' ,
            \    'cyan'                    : '#00538b' ,
            \    'cyan_alt'                : '#30517f' ,
            \    'cyan_alt_other'          : '#005a5f' ,
            \    'cyan_faint'              : '#005077' ,
            \    'cyan_alt_faint'          : '#354f6f' ,
            \    'cyan_alt_other_faint'    : '#125458' ,
            \    'red_intense'             : '#b60000' ,
            \    'orange_intense'          : '#904200' ,
            \    'green_intense'           : '#006800' ,
            \    'yellow_intense'          : '#605b00' ,
            \    'blue_intense'            : '#1f1fce' ,
            \    'magenta_intense'         : '#a8007f' ,
            \    'purple_intense'          : '#7f10d0' ,
            \    'cyan_intense'            : '#005f88' ,
            \    'red_active'              : '#8a0000' ,
            \    'green_active'            : '#004c2e' ,
            \    'yellow_active'           : '#702d1f' ,
            \    'blue_active'             : '#0030b4' ,
            \    'magenta_active'          : '#5c2092' ,
            \    'cyan_active'             : '#003f8a'
            \ }

function! s:Highlight(group, fg, bg, style) abort
    const l:cmd = 'highlight '   .. a:group
                \ .. ' guifg=' .. a:fg
                \ .. ' guibg=' .. a:bg
                \ .. ' gui='   .. a:style
                \ .. ' cterm=' .. a:style
    execute l:cmd
endfunction

function! s:SetHighlights() abort
    call s:Highlight('StatusBlank',      s:colors.fg_main,        s:colors.bg_main,   'NONE')
    call s:Highlight('StatusFileName',   s:colors.fg_active,      s:colors.bg_main,   'bold')
    call s:Highlight('StatusFileState',  s:colors.purple_intense, s:colors.bg_main,   'bold')
    call s:Highlight('StatusFileSize',   s:colors.fg_main,        s:colors.bg_main,   'NONE')
    call s:Highlight('StatusFileFormat', s:colors.fg_active,      s:colors.bg_active, 'NONE')
    call s:Highlight('StatusFileEncode', s:colors.fg_active,      s:colors.bg_active, 'NONE')
    call s:Highlight('StatusPercent',    s:colors.fg_main,        s:colors.bg_main,   'NONE')
endfunction

function! s:GetVimMode() abort
    " :help mode()
    const l:modes = {
                \ 'n'    : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:colors.blue           },
                \ 'no'   : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:colors.yellow         },
                \ 'nov'  : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:colors.yellow         },
                \ 'noV'  : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:colors.yellow         },
                \ 'no' : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:colors.yellow         },
                \ 'niI'  : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:colors.blue           },
                \ 'niR'  : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:colors.blue           },
                \ 'niV'  : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:colors.blue           },
                \ 'nt'   : { 'abbrev': 'T',  'alias': 'TERM',      'color' : s:colors.fg_active      },
                \ 'v'    : { 'abbrev': 'V',  'alias': 'VISUAL',    'color' : s:colors.cyan           },
                \ 'vs'   : { 'abbrev': 'V',  'alias': 'VISUAL',    'color' : s:colors.cyan           },
                \ 'V'    : { 'abbrev': 'Vl', 'alias': 'V·LINE',    'color' : s:colors.cyan           },
                \ 'Vs'   : { 'abbrev': 'Vl', 'alias': 'V·LINE',    'color' : s:colors.cyan           },
                \ ''   : { 'abbrev': 'Vb', 'alias': 'V·BLOCK',   'color' : s:colors.cyan           },
                \ 's'  : { 'abbrev': 'Vb', 'alias': 'V·BLOCK',   'color' : s:colors.cyan           },
                \ 's'    : { 'abbrev': 'S',  'alias': 'SELECT',    'color' : s:colors.blue           },
                \ 'S'    : { 'abbrev': 'Sl', 'alias': 'S·LINE',    'color' : s:colors.blue           },
                \ ''   : { 'abbrev': 'Sb', 'alias': 'S·BLOCK',   'color' : s:colors.blue           },
                \ 'i'    : { 'abbrev': 'I',  'alias': 'INSERT',    'color' : s:colors.green          },
                \ 'ic'   : { 'abbrev': 'I',  'alias': 'INSERT',    'color' : s:colors.green          },
                \ 'ix'   : { 'abbrev': 'I',  'alias': 'INSERT',    'color' : s:colors.green          },
                \ 'R'    : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:colors.red            },
                \ 'Rc'   : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:colors.red            },
                \ 'Rx'   : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:colors.red            },
                \ 'Rv'   : { 'abbrev': 'Vr', 'alias': 'V·REPLACE', 'color' : s:colors.red            },
                \ 'Rvc'  : { 'abbrev': 'Vr', 'alias': 'V·REPLACE', 'color' : s:colors.red            },
                \ 'Rvx'  : { 'abbrev': 'Vr', 'alias': 'V·REPLACE', 'color' : s:colors.red            },
                \ 'c'    : { 'abbrev': 'C',  'alias': 'COMMAND',   'color' : s:colors.fg_main        },
                \ 'cv'   : { 'abbrev': 'C',  'alias': 'EX',        'color' : s:colors.fg_main        },
                \ 'ce'   : { 'abbrev': 'C',  'alias': 'EX',        'color' : s:colors.fg_main        },
                \ 'r'    : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:colors.red            },
                \ 'rm'   : { 'abbrev': 'Rm', 'alias': 'MORE',      'color' : s:colors.purple_intense },
                \ 'r?'   : { 'abbrev': 'R?', 'alias': 'CONFIRM',   'color' : s:colors.purple_intense },
                \ '!'    : { 'abbrev': 'Sh', 'alias': 'SHELL',     'color' : s:colors.fg_active      },
                \ 't'    : { 'abbrev': 'T',  'alias': 'TERM',      'color' : s:colors.fg_active      }
                \ }

    const l:mode = mode()
    call s:Highlight('StatusVimMode', s:colors.bg_main, l:modes[l:mode].color, 'bold')

    return l:modes[l:mode].abbrev
endfunction

function! s:GetGitBranch() abort
    const l:git_icon = ' '
    const l:git_dir = expand('%:p:h:S')
    const l:git_cmd = 'git -C ' .. git_dir .. ' status --branch --porcelain=2'
    :silent! const l:git_cmd_result = system(l:git_cmd)

    " Line                                     Notes
    " ------------------------------------------------------------
    " # branch.oid <commit> | (initial)        Current commit.
    " # branch.head <branch> | (detached)      Current branch.
    " # branch.upstream <upstream_branch>      If upstream is set.
    " # branch.ab +<ahead> -<behind>           If upstream is set and
    "                                          the commit is present.
    " ------------------------------------------------------------

    if v:shell_error
        call s:Highlight('StatusGitBranch', s:colors.fg_main, s:colors.bg_main, 'NONE')
        return ''
    else
        let l:git_status = l:git_cmd_result->split('\n')
        let l:git_branch = l:git_status[1]->split()[2]

        if l:git_status->len() > 1
            " dirty branch
            let l:git_color = s:colors.red_active
        else
            " clean branch
            let l:git_color = s:colors.green_active
        end

        call s:Highlight('StatusGitBranch', l:git_color, s:colors.bg_main, 'NONE')
        return l:git_icon .. l:git_branch
    end
endfunction

function! s:GetFileSize() abort
    let l:file = expand('%:p')
    let l:bytes = 0

    if len(l:file) <= 0
        " it's a buffer
        let l:bytes = wordcount().bytes
    else
        let l:bytes = getfsize(l:file)
    endif

    if l:bytes == 0 || l:bytes == -1 || l:bytes == -2
        return ''
    end

    let l:_1K = 1024
    let l:_1M = 1024 * l:_1K
    let l:_1G = 1024 * l:_1M
    let l:_1T = 1024 * l:_1G
    let l:_1P = 1024 * l:_1T
    let l:_1E = 1024 * l:_1P

    let l:size=''
    if l:bytes < l:_1K
        let l:size = printf('%dB',   l:bytes)
    elseif l:bytes < l:_1M
        let l:size = printf('%.1fK', l:bytes/l:_1K)
    elseif l:bytes < l:_1G
        let l:size = printf('%.1fM', l:bytes/l:_1M)
    elseif l:bytes < l:_1T
        let l:size = printf('%.1fG', l:bytes/l:_1G)
    elseif l:bytes < l:_1P
        let l:size = printf('%.1fT', l:bytes/l:_1T)
    elseif l:bytes < l:_1E
        let l:size = printf('%.1fP', l:bytes/l:_1P)
    else " math.maxinteger = 2^63 -1
        let l:size = printf('%.1fE', l:bytes/l:_1E)
    end

    return l:size
endfunction

function! BuildStatusLine() abort
    const l:focus = g:statusline_winid == win_getid()

    if !l:focus
        let l:statusline = join ([
                    \  '%#StatusFileName#',
                    \  ' ',
                    \  '%<%t',
                    \  '%=',
                    \  '%#StatusPercent#',
                    \  '%P'
                    \  ], '')
    else
        let l:statusline = join ([
                    \  '%#StatusVimMode#',
                    \  ' ',
                    \  s:GetVimMode(),
                    \  ' ',
                    \  '%#StatusBlank#',
                    \  ' ',
                    \  '%#StatusFileName#',
                    \  '%<%F',
                    \  '%#StatusBlank#',
                    \  ' ',
                    \  '%#StatusFileState#',
                    \  '%m%r%h%w%q',
                    \  '%#StatusBlank#',
                    \  ' ',
                    \  '%#StatusGitBranch#',
                    \  s:GetGitBranch(),
                    \  '%#StatusBlank#',
                    \  '%=',
                    \  '%#StatusFileSize#',
                    \  s:GetFileSize(),
                    \  '%#StatusBlank#',
                    \  ' ',
                    \  '%#StatusFileFormat#',
                    \  ' ',
                    \  '%{&fileformat}',
                    \  ' | ',
                    \  '%#StatusFileEncode#',
                    \  '%{&fileencoding ? &fileencoding : &encoding}',
                    \  ' ',
                    \  '%#StatusBlank#',
                    \  ' ',
                    \  '%#StatusPercent#',
                    \  '%P'
                    \  ], '')
    endif

    return l:statusline
endfunction

call s:SetHighlights()
set statusline=%!BuildStatusLine()

augroup StatusLine | autocmd!
    autocmd ColorScheme,BufEnter,BufWinEnter,WinEnter * call s:SetHighlights()
    autocmd VimResized *  redrawstatus
augroup END
