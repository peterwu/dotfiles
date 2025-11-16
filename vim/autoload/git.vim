vim9script

export def Log(): list<string>
    const output = system('git log --oneline --decorate --color=always --all')

    return v:shell_error != 0
        ? []
        : output->split('\n')->filter((_, line) => !line->empty())
enddef

export def Show(commit: string): void
    const commit_hash = commit ?? 'HEAD'

    enew
    setlocal buftype=nofile bufhidden=wipe noswapfile nobuflisted

    execute $'silent! read !git show {commit_hash}'
    :1delete _

    setlocal filetype=git nomodifiable
    execute $'file git-show:{commit_hash}'
enddef
