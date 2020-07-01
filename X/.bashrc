#
# ~/.bashrc
#

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

set -o vi

alias emacs='emacsclient --tty --quiet' 
alias e='emacs'

alias ls='ls --color=auto' # use colors
alias la='ls -Flsa'        # list all files
alias ll='ls -Fls'         # long listing format

alias rm='rm -i'           # prompt before overwrite (but dangerous, see rm for a better approach)
alias cp='cp -i'           # prompt before overwrite (same general problem as the rm)
alias mv='mv -i'           # prompt before overwrite (same general problem as the rm)

alias bc='bc -l'

PS1='[\u@\h \W]\$ '

export HISTSIZE=20000
export HISTFILESIZE=20000

export LESSHISTFILE=-
export LESS='--mouse --wheel-lines=3'

# nnn
export NNN_BMS='d:~/Documents;j:~/Downloads;p:~/Projects'
export NNN_COLORS="4236"                           # use a different color for each context

# lightdm doesn't source .xprofile
source ~/.xprofile
