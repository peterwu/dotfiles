# Emacs wrapper (macOS app-bundle paths; only keep this file on machines that have it)
_emacs() {
    local emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
    local emacs_nw="/Applications/Emacs.app/Contents/MacOS/emacs-nw"

    if [ ! -x "$emacsclient" ]; then
        echo "Error: emacsclient not found or not executable at $emacsclient" >&2
        return 1
    fi

    if [ ! -x "$emacs_nw" ]; then
        echo "Error: emacs-nw not found or not executable at $emacs_nw" >&2
        return 1
    fi

    "$emacsclient" --quiet \
        --no-window-system \
        --alternate-editor="$emacs_nw" "$@"
}

alias e="_emacs"
