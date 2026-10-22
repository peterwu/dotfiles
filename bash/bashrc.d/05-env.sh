# Environment variables (exported, so child processes inherit them)

export CDPATH=".:$HOME"

# XDG directories
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

# editor
export EDITOR='vim'

# manpager
export MANPAGER='vim +MANPAGER +"set showtabline=0" --not-a-term -'

# less
export LESSHISTFILE=-

# go
export GOPATH=$HOME/.local/share/go

# readline
export INPUTRC=$HOME/.config/readline/inputrc

# pass
export PASSWORD_STORE_DIR=$HOME/.local/share/pass
export PASSWORD_STORE_GENERATED_LENGTH=20
