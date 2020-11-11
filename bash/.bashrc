# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi

# User specific aliases and functions
set -o vi

alias se='sudoedit'

alias ls='ls --color' # use colors
alias la='ls -Flsa'   # list all files
alias ll='ls -Fls'    # long listing format

alias rm='rm -i'      # prompt before overwrite (but dangerous, see rm for a better approach)
alias cp='cp -i'      # prompt before overwrite (same general problem as the rm)
alias mv='mv -i'      # prompt before overwrite (same general problem as the rm)

alias bc='bc -l'

alias man='GROFF_NO_SGR=1 man'

# PS1='[\u@\h \W]\$ '
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

COLOR_RED="$(tput setaf 1)"
COLOR_GREEN="$(tput setaf 2)"
COLOR_MAGENTA="$(tput setaf 5)"
RESET="$(tput sgr0)"
PS1='${COLOR_MAGENTA}* ${COLOR_GREEN}\w ${COLOR_RED}$(parse_git_branch)${RESET}\n\$ '

HISTSIZE=20000
HISTFILESIZE=20000

# fzf
source /usr/share/fzf/shell/key-bindings.bash

