#
# ~/.bash_profile
#

# Get the aliases and functions
[[ -f ~/.bashrc ]] && . ~/.bashrc

# User specific environment and startup programs
export CDPATH=".:$HOME"

# readline
export INPUTRC=$HOME/.config/readline/inputrc

# less
export LESSHISTFILE=-

# python
export PYTHONSTARTUP=$HOME/.config/python/pythonrc

# fzf
export FZF_DEFAULT_COMMAND='find .'
export FZF_DEFAULT_OPTS='--height 100% --reverse --border --multi'
