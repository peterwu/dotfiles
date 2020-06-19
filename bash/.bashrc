#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

set -o vi

alias emacs='emacsclient --tty --quiet' 
# alias vi='emacs'

alias ls='ls --color=auto' # use colors
alias la='ls -Flsa'        # list all files
alias ll='ls -Fls'         # long listing format

alias rm='rm -i'           # prompt before overwrite (but dangerous, see rm for a better approach)
alias cp='cp -i'           # prompt before overwrite (same general problem as the rm)
alias mv='mv -i'           # prompt before overwrite (same general problem as the rm)

alias bc='bc -l'

PS1='[\u@\h \W]\$ '

HISTSIZE=20000
HISTFILESIZE=20000

export LESS='--mouse --wheel-lines=3'
