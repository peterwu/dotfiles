# MacVim (macOS only)
MACVIM_BIN="/Applications/MacVim.app/Contents/bin"

if [ -x "$MACVIM_BIN/vim" ]; then
    _path_prepend "$MACVIM_BIN"
fi
