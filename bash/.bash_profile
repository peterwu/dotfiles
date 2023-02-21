#
# ~/.bash_profile
#

# Get the aliases and functions
[[ -f ~/.bashrc ]] && source ~/.bashrc

# startx if possible
[[ -z "${DISPLAY}" && "${XDG_VTNR}" -eq 1 ]] && exec /usr/local/bin/welcome

# User specific environment and startup programs
export CDPATH=".:$HOME"

# less
export LESSHISTFILE=-

# python
export PYTHONSTARTUP=$HOME/.config/python/pythonrc

# readline
export INPUTRC=$HOME/.config/readline/inputrc
