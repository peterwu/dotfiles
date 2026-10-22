# bash completion via Homebrew (interactive shells only)
[[ $- == *i* ]] || return

if [[ -n "$HOMEBREW_PREFIX" && -r "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh" ]]; then
    source "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"
fi
