" -----------
" Status Line
" -----------
" modus operandi color palette
const s:palette = {
            "\ Basic values
            \  'bg_main':            "#ffffff",
            \  'bg_dim':             "#f0f0f0",
            \  'fg_main':            "#000000",
            \  'fg_dim':             "#595959",
            \  'fg_alt':             "#193668",
            \  'bg_active':          "#c4c4c4",
            \  'bg_inactive':        "#e0e0e0",
            \  'border':             "#9f9f9f",
            \
            "\ Common accent foregrounds
            \  'red':                "#a60000",
            \  'red_warmer':         "#972500",
            \  'red_cooler':         "#a0132f",
            \  'red_faint':          "#7f0000",
            \  'red_intense':        "#d00000",
            \  'green':              "#006800",
            \  'green_warmer':       "#316500",
            \  'green_cooler':       "#00663f",
            \  'green_faint':        "#2a5045",
            \  'green_intense':      "#008900",
            \  'yellow':             "#6f5500",
            \  'yellow_warmer':      "#884900",
            \  'yellow_cooler':      "#7a4f2f",
            \  'yellow_faint':       "#624416",
            \  'yellow_intense':     "#808000",
            \  'blue':               "#0031a9",
            \  'blue_warmer':        "#3548cf",
            \  'blue_cooler':        "#0000b0",
            \  'blue_faint':         "#003497",
            \  'blue_intense':       "#0000ff",
            \  'magenta':            "#721045",
            \  'magenta_warmer':     "#8f0075",
            \  'magenta_cooler':     "#531ab6",
            \  'magenta_faint':      "#7c318f",
            \  'magenta_intense':    "#dd22dd",
            \  'cyan':               "#005e8b",
            \  'cyan_warmer':        "#3f578f",
            \  'cyan_cooler':        "#005f5f",
            \  'cyan_faint':         "#005077",
            \  'cyan_intense':       "#008899",
            \
            "\ Uncommon accent foregrounds
            \  'rust':               "#8a290f",
            \  'gold':               "#80601f",
            \  'olive':              "#56692d",
            \  'slate':              "#2f3f83",
            \  'indigo':             "#4a3a8a",
            \  'maroon':             "#731c52",
            \  'pink':               "#7b435c",
            \
            "\ Common accent backgrounds
            \  'bg_red_intense':     "#ff8f88",
            \  'bg_green_intense':   "#8adf80",
            \  'bg_yellow_intense':  "#f3d000",
            \  'bg_blue_intense':    "#bfc9ff",
            \  'bg_magenta_intense': "#dfa0f0",
            \  'bg_cyan_intense':    "#a4d5f9",
            \
            \  'bg_red_subtle':      "#ffcfbf",
            \  'bg_green_subtle':    "#b3fabf",
            \  'bg_yellow_subtle':   "#fff576",
            \  'bg_blue_subtle':     "#ccdfff",
            \  'bg_magenta_subtle':  "#ffddff",
            \  'bg_cyan_subtle':     "#bfefff",
            \
            \  'bg_red_nuanced':     "#fff1f0",
            \  'bg_green_nuanced':   "#ecf7ed",
            \  'bg_yellow_nuanced':  "#fff3da",
            \  'bg_blue_nuanced':    "#f3f3ff",
            \  'bg_magenta_nuanced': "#fdf0ff",
            \  'bg_cyan_nuanced':    "#ebf6fa",
            \
            "\ Uncommon accent backgrounds
            \  'bg_ochre':           "#f0e0cc",
            \  'bg_lavender':        "#dfdbfa",
            \  'bg_sage':            "#c0e7d4"
            \ }

function! s:Highlight(group, fg, bg, style) abort
    const l:cmd = 'highlight ' .. a:group
                \ .. ' guifg=' .. a:fg
                \ .. ' guibg=' .. a:bg
                \ .. ' gui='   .. a:style
                \ .. ' cterm=' .. a:style
    execute l:cmd
endfunction

function! s:SetHighlights() abort
    call s:Highlight('StatusBlank',      s:palette.fg_main, s:palette.bg_blue_nuanced, 'NONE')
    call s:Highlight('StatusFileName',   s:palette.fg_main, s:palette.bg_blue_nuanced, 'bold')
    call s:Highlight('StatusFileState',  s:palette.maroon,  s:palette.bg_blue_nuanced, 'bold')
    call s:Highlight('StatusFileSize',   s:palette.fg_main, s:palette.bg_blue_nuanced, 'NONE')
    call s:Highlight('StatusFileFormat', s:palette.fg_main, s:palette.bg_lavender,     'NONE')
    call s:Highlight('StatusFileEncode', s:palette.fg_main, s:palette.bg_lavender,     'NONE')
    call s:Highlight('StatusPercent',    s:palette.fg_main, s:palette.bg_blue_nuanced, 'NONE')
endfunction

function! s:GetVimMode() abort
    " :help mode()
    const l:modes = {
                \ 'n'    : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:palette.blue    },
                \ 'no'   : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:palette.yellow  },
                \ 'nov'  : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:palette.yellow  },
                \ 'noV'  : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:palette.yellow  },
                \ 'no' : { 'abbrev': 'O',  'alias': 'O·PENDING', 'color' : s:palette.yellow  },
                \ 'niI'  : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:palette.blue    },
                \ 'niR'  : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:palette.blue    },
                \ 'niV'  : { 'abbrev': 'N',  'alias': 'NORMAL',    'color' : s:palette.blue    },
                \ 'nt'   : { 'abbrev': 'T',  'alias': 'TERM',      'color' : s:palette.fg_main },
                \ 'v'    : { 'abbrev': 'V',  'alias': 'VISUAL',    'color' : s:palette.cyan    },
                \ 'vs'   : { 'abbrev': 'V',  'alias': 'VISUAL',    'color' : s:palette.cyan    },
                \ 'V'    : { 'abbrev': 'Vl', 'alias': 'V·LINE',    'color' : s:palette.cyan    },
                \ 'Vs'   : { 'abbrev': 'Vl', 'alias': 'V·LINE',    'color' : s:palette.cyan    },
                \ ''   : { 'abbrev': 'Vb', 'alias': 'V·BLOCK',   'color' : s:palette.cyan    },
                \ 's'  : { 'abbrev': 'Vb', 'alias': 'V·BLOCK',   'color' : s:palette.cyan    },
                \ 's'    : { 'abbrev': 'S',  'alias': 'SELECT',    'color' : s:palette.blue    },
                \ 'S'    : { 'abbrev': 'Sl', 'alias': 'S·LINE',    'color' : s:palette.blue    },
                \ ''   : { 'abbrev': 'Sb', 'alias': 'S·BLOCK',   'color' : s:palette.blue    },
                \ 'i'    : { 'abbrev': 'I',  'alias': 'INSERT',    'color' : s:palette.green   },
                \ 'ic'   : { 'abbrev': 'I',  'alias': 'INSERT',    'color' : s:palette.green   },
                \ 'ix'   : { 'abbrev': 'I',  'alias': 'INSERT',    'color' : s:palette.green   },
                \ 'R'    : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:palette.red     },
                \ 'Rc'   : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:palette.red     },
                \ 'Rx'   : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:palette.red     },
                \ 'Rv'   : { 'abbrev': 'Vr', 'alias': 'V·REPLACE', 'color' : s:palette.red     },
                \ 'Rvc'  : { 'abbrev': 'Vr', 'alias': 'V·REPLACE', 'color' : s:palette.red     },
                \ 'Rvx'  : { 'abbrev': 'Vr', 'alias': 'V·REPLACE', 'color' : s:palette.red     },
                \ 'c'    : { 'abbrev': 'C',  'alias': 'COMMAND',   'color' : s:palette.rust    },
                \ 'cv'   : { 'abbrev': 'C',  'alias': 'EX',        'color' : s:palette.rust    },
                \ 'ce'   : { 'abbrev': 'C',  'alias': 'EX',        'color' : s:palette.rust    },
                \ 'r'    : { 'abbrev': 'R',  'alias': 'REPLACE',   'color' : s:palette.red     },
                \ 'rm'   : { 'abbrev': 'Rm', 'alias': 'MORE',      'color' : s:palette.yellow  },
                \ 'r?'   : { 'abbrev': 'R?', 'alias': 'CONFIRM',   'color' : s:palette.yellow  },
                \ '!'    : { 'abbrev': 'Sh', 'alias': 'SHELL',     'color' : s:palette.fg_main },
                \ 't'    : { 'abbrev': 'T',  'alias': 'TERM',      'color' : s:palette.fg_main }
                \ }

    const l:mode = mode()
    call s:Highlight('StatusVimMode', s:palette.bg_main, l:modes[l:mode].color, 'bold')
    return l:modes[l:mode].abbrev
endfunction

function! s:GetGitBranch() abort
    const l:git_icon = ''
    const l:git_dir = expand('%:p:h:S')
    const l:git_cmd = 'git -C ' .. git_dir .. ' status --branch --porcelain=2'
    silent! let l:git_cmd_result = system(l:git_cmd)->split('\n')

    " Line                                     Notes
    " ------------------------------------------------------------
    " # branch.oid <commit> | (initial)        Current commit.
    " # branch.head <branch> | (detached)      Current branch.
    " # branch.upstream <upstream_branch>      If upstream is set.
    " # branch.ab +<ahead> -<behind>           If upstream is set and
    "                                          the commit is present.
    " ------------------------------------------------------------

    if v:shell_error
        call s:Highlight('StatusGitBranch', s:palette.fg_main, s:palette.bg_blue_nuanced, 'NONE')
        return ''
    else
        let l:git_branch = l:git_cmd_result->filter('v:val =~ "^# branch.head"')[0]->split()[2]
        let l:git_status = l:git_cmd_result->filter('v:val !~ "^# "')

        if l:git_status->len() > 0
            " dirty branch
            let l:git_color = s:palette.red_faint
        else
            " clean branch
            let l:git_color = s:palette.green_faint
        end

        call s:Highlight('StatusGitBranch', l:git_color, s:palette.bg_blue_nuanced, 'NONE')
        return l:git_icon .. ' ' .. l:git_branch
    end
endfunction

function! s:GetFileSize() abort
    let l:file = expand('%:p')
    let l:bytes = 0

    if l:file->len() <= 0
        " it's a buffer
        let l:bytes = wordcount().bytes
    else
        let l:bytes = l:file->getfsize()
    endif

    if l:bytes == 0 || l:bytes == -1 || l:bytes == -2
        return ''
    end

    const l:_1K = 1024
    const l:_1M = 1024 * l:_1K
    const l:_1G = 1024 * l:_1M
    const l:_1T = 1024 * l:_1G
    const l:_1P = 1024 * l:_1T
    const l:_1E = 1024 * l:_1P

    if l:bytes < l:_1K
        return printf('%dB',   l:bytes)
    elseif l:bytes < l:_1M
        return printf('%.1fK', l:bytes/l:_1K)
    elseif l:bytes < l:_1G
        return printf('%.1fM', l:bytes/l:_1M)
    elseif l:bytes < l:_1T
        return printf('%.1fG', l:bytes/l:_1G)
    elseif l:bytes < l:_1P
        return printf('%.1fT', l:bytes/l:_1T)
    elseif l:bytes < l:_1E
        return printf('%.1fP', l:bytes/l:_1P)
    else " math.maxinteger = 2^63 -1
        return printf('%.1fE', l:bytes/l:_1E)
    end
endfunction

function! BuildStatusLine() abort
    if g:statusline_winid != win_getid()
        return join ([
                    \  '%#StatusFileName#',
                    \  ' ',
                    \  '%<%t',
                    \  '%=',
                    \  '%#StatusPercent#',
                    \  '%P'
                    \  ], '')
    else
        return join ([
                    \    '%#StatusVimMode#',
                    \    ' ',
                    \    s:GetVimMode(),
                    \    ' ',
                    \    '%#StatusBlank#',
                    \    ' ',
                    \    '%#StatusFileName#',
                    \    '%<%F',
                    \    '%#StatusBlank#',
                    \    ' ',
                    \    '%#StatusFileState#',
                    \    '%m%r%h%w%q',
                    \    '%#StatusBlank#',
                    \    ' ',
                    \    '%#StatusGitBranch#',
                    \    s:GetGitBranch(),
                    \    '%#StatusBlank#',
                    \    '%=',
                    \    '%#StatusFileSize#',
                    \    s:GetFileSize(),
                    \    '%#StatusBlank#',
                    \    ' ',
                    \    '%#StatusFileFormat#',
                    \    ' ',
                    \    '%{&fileformat}',
                    \    ' | ',
                    \    '%#StatusFileEncode#',
                    \    '%{&fileencoding ? &fileencoding : &encoding}',
                    \    ' ',
                    \    '%#StatusBlank#',
                    \    ' ',
                    \    '%#StatusPercent#',
                    \    '%P'
                    \ ], '')
    endif
endfunction

call s:SetHighlights()
set statusline=%!BuildStatusLine()

augroup StatusLine | autocmd!
    autocmd ColorScheme,BufEnter,BufWinEnter,WinEnter * call s:SetHighlights()
    autocmd VimResized *  redrawstatus
augroup END
