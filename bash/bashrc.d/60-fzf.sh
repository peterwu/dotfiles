# fzf key bindings and completion (interactive shells only)
[[ $- == *i* ]] || return

if type fzf &>/dev/null; then
    eval "$(fzf --bash)"

    export FZF_DEFAULT_OPTS='
    --color bg:#ffffff,bg+:#f0f0f0,fg:#000000,fg+:#000000,hl:#354fcf,hl+:#531ab6
    --height ~9 --reverse --style minimal --no-info
    --preview "cat {}"
    --preview-window right:60%:hidden:wrap
    --bind ctrl-y:toggle-preview
    '
fi
