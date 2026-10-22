# Homebrew and GNU coreutils (only keep on machines that have brew)
if type brew &>/dev/null; then
    # `brew shellenv` is slow and its exports are inherited by child shells,
    # so only run it when HOMEBREW_PREFIX isn't already set.
    [[ -n "$HOMEBREW_PREFIX" ]] || eval "$(brew shellenv)"
    _path_prepend "$HOMEBREW_PREFIX/sbin"
    _path_prepend "$HOMEBREW_PREFIX/bin"

    # GNU coreutils
    if [[ -d "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin" ]]; then
        _path_prepend "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin"
    fi
fi
