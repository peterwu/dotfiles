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

# nnn
export NNN_BMS='d:~/Documents;j:~/Downloads;p:~/Projects'
export NNN_COLORS='4152'
export NNN_FCOLORS='c1e2272e006033f7c6d6abc4' 
export NNN_PLUG='f:fzcd;'

# fzf
export FZF_DEFAULT_COMMAND='find .'
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --multi --color=light --preview "(highlight -O ansi -l {} || cat {} || tree -C {}) 2>/dev/null | head -100"'
export FZF_ALT_C_OPTS='--preview "tree -C {} | head -100"'

# go
# export GOPATH=$HOME/.local/share/go
# export PATH=$PATH:$GOPATH/bin

# rust
# export CARGO_HOME=$HOME/.local/share/cargo
# export RUSTUP_HOME=$HOME/.local/share/rustup
# export PATH=$PATH:$CARGO_HOME/bin

