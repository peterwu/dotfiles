#
# ~/.bash_profile
#

# Get the aliases and functions
[[ -f ~/.bashrc ]] && . ~/.bashrc

# User specific environment and startup programs
export CDPATH=".:$HOME"

# less
export LESSHISTFILE=-
export LESS='-R --mouse --wheel-lines=3'

# go & rust
export GOPATH=$HOME/.local/share/go
export CARGO_HOME=$HOME/.local/share/cargo

