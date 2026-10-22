# PATH helper: put a directory at the front of PATH, removing any existing
# copy first. Safe to run repeatedly (nested shells, re-sourcing) without
# piling up duplicates, and re-asserts priority if something reordered PATH.
_path_prepend() {
    local p=":${PATH}:"
    p=${p//:"$1":/:}
    p=${p//:"$1":/:}
    p=${p#:}
    p=${p%:}
    PATH="$1${p:+:$p}"
}

# User specific environment
_path_prepend "$HOME/bin"
_path_prepend "$HOME/.local/bin"
