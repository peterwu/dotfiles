# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
export CDPATH=".:$HOME"

# less
export LESSHISTFILE=-
export LESS='-R --mouse --wheel-lines=3'

# fzf
export FZF_DEFAULT_COMMAND='find .'
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --multi --color=light --preview "(highlight -O ansi -l {} || cat {} || tree -C {}) 2>/dev/null | head -100"'
export FZF_ALT_C_OPTS='--preview "tree -C {} | head -100"'
