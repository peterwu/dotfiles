vim9script

export def Yank(arg = ''): string
    &operatorfunc = (type: string, ...args: list<string>) => {
        const selection = &selection
        const clipboard = &clipboard
        const register = getreginfo(v:register)
        const visual_marks = [getpos("'<"), getpos("'>")]

        set selection=inclusive

        if type == 'char'
            silent execute 'normal!' $'v`[o`]"{v:register}y'
        elseif type == 'line'
            silent execute 'normal!' $'`[V`]"{v:register}y'
        elseif type == 'block' || type == 'v' || type == 'V' || type == "\<C-V>"
            silent execute 'normal!' $'gv"{v:register}y'
        elseif type =~ '^\d\+$'
            silent execute 'normal!' $'^v{type}$h"{v:register}y'
            if mode() == 'v'
                normal! v
            endif
        endif

        const orig_text = getreg(v:register)
        const b64_text = orig_text->split('\n')->str2blob()->base64_encode()
        const osc52_text = $"\x1b]52;c;{b64_text}\x07"

        echoraw(osc52_text)

        &selection = selection
        &clipboard = clipboard
        setreg(v:register, register)
        setpos("'<", visual_marks[0])
        setpos("'>", visual_marks[1])
    }

    return $'g@{arg}'
enddef

export def Paste(command: string)
    b:pending_paste_command = command

    autocmd TermResponseAll osc ++once {
        const b64text = matchstr(v:termosc, '^\%x1b]52;.\+;\zs[A-Za-z0-9+/=]\+')
        if !empty(b64text)
            const decoded_text = b64text->base64_decode()->blob2str()
            setreg(v:register, decoded_text)
            execute 'normal!' $'"{v:register}{b:pending_paste_command}'
        endif
    }

    echoraw("\x1b]52;c;?\x07")
enddef

