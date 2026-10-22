# Aliases
_define_aliases() {
    alias ls="ls -v               \
        --color=auto              \
        --group-directories-first \
        --indicator-style=slash   \
        --time-style=long-iso"
    alias ll="ls -l --classify --size"
    alias la="ls -l --classify --size --all"

    alias rm="rm --interactive"
    alias cp="cp --interactive"
    alias mv="mv --interactive"

    alias bc="bc --mathlib"

    alias diff="diff --color=auto"

    alias grep="grep --color=auto"
    alias egrep="grep -E --color=auto"
    alias fgrep="grep -F --color=auto"

    alias less="LESSHISTFILE=- less -FXR --mouse --wheel-lines=3"
    alias rsync="rsync --progress"
}

_define_aliases
