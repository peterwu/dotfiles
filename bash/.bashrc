#
# ~/.bashrc
#

# Source global definitions
if [ -f /etc/bash.bashrc ]; then
  . /etc/bash.bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
  PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

export CDPATH=".:$HOME"

man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}
# export MANPAGER='nvim +Man!'

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

set -o vi

alias vi='nvim'
alias se='sudoedit'
alias e='emacsclient --tty --quiet' 
# alias vi='emacsclient --tty --quiet' 
# export EDITOR='emacsclient --tty --quiet'

alias ls='ls --color' # use colors
alias la='ls -Flsa'   # list all files
alias ll='ls -Fls'    # long listing format

alias rm='rm -i'      # prompt before overwrite (but dangerous, see rm for a better approach)
alias cp='cp -i'      # prompt before overwrite (same general problem as the rm)
alias mv='mv -i'      # prompt before overwrite (same general problem as the rm)

alias bc='bc -l'

# PS1='[\u@\h \W]\$ '
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

COLOR_RED="$(tput setaf 1)"
COLOR_GREEN="$(tput setaf 2)"
COLOR_MAGENTA="$(tput setaf 5)"
RESET="$(tput sgr0)"
PS1='${COLOR_MAGENTA}* ${COLOR_GREEN}\w ${COLOR_RED}$(parse_git_branch)${RESET}\n\$ '

export HISTSIZE=20000
export HISTFILESIZE=20000

export LESSHISTFILE=-
export LESS='-R --mouse --wheel-lines=3'

# fzf
source /usr/share/fzf/key-bindings.bash
