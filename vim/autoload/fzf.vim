vim9script

export def Tabs()
    const selected_tab_nr = tabpagenr()
    const last_tab_nr = tabpagenr('$')

    const tabs = range(1, last_tab_nr)->mapnew((_, i) => {
        const label = g:GetTabLabel(i)
        return (i == selected_tab_nr ? '*' : ' ') .. label
    })

    fzf#run(fzf#wrap({
        source: tabs,
        sink: (line) => {
            const tabnum = matchstr(line, '^\s*\*\?\s*\zs\d\+')
            execute 'tabnext' tabnum
        },
        options: '--prompt "Tab> "'
    }))
enddef
