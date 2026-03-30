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

export def GitCommits()
    const logs = GitLog()

    fzf#run(fzf#wrap({
        source: logs,
        sink: (line) => {
            const commit = matchstr(line, '^\s*\*\?\s*\zs\w\+')
            GitShow(commit)
        },
        options: '--prompt "Git Log> " --ansi'
    }))
enddef

def GitLog(): list<string>
    const output = system('git log --oneline --decorate --color=always --all')

    return v:shell_error != 0
        ? []
        : output->split('\n')->filter((_, line) => !line->empty())
enddef

def GitShow(commit: string): void
    const commit_hash = commit ?? 'HEAD'

    enew
    setlocal buftype=nofile bufhidden=wipe noswapfile nobuflisted

    execute $'silent! read !git show {commit_hash}'
    :1delete _

    setlocal filetype=git nomodifiable
    execute $'file git-show:{commit_hash}'
enddef
