vim9script

export def ToggleWindow(type: string): void
    const [value, prefix] = type == 'qf'
        ? ['v:val.quickfix', 'c']
        : ['v:val.loclist',  'l']

    const cmd = getwininfo()->filter(value)->empty() ? 'open' : 'close'
    execute 'silent!' $'{prefix}{cmd}'
enddef

export def Grep(...params: list<string>): string
    const args = params->join(' ')->expandcmd()
    const cmd = &grepprg->substitute('\$\*', args, '')
    return system(cmd)
enddef

