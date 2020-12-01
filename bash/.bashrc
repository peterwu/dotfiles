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
alias vi='vim'

alias emacs='emacsclient --tty --quiet'
alias e='emacs'

alias ls='ls --color' # use colors
alias la='ls -Flsa'   # list all files
alias ll='ls -Fls'    # long listing format

alias rm='rm -i'      # prompt before overwrite (but dangerous, see rm for a better approach)
alias cp='cp -i'      # prompt before overwrite (same general problem as the rm)
alias mv='mv -i'      # prompt before overwrite (same general problem as the rm)

alias bc='bc -l'

alias man='GROFF_NO_SGR=1 man'

HISTSIZE=20000
HISTFILESIZE=20000

show_bash_prompt() {
  # The entire table of ANSI color codes
  # https://gist.github.com/iamnewton/8754917
  # \### format must be used in functions
  # \001 == \[
  # \002 == \]
  # \033 == \e

  local last_command_status=$?
  local prompt=''

  # current time
  prompt+="\001\e[0;93m\002"
  prompt+="$(date +%R)"
  prompt+=' '

  # user@host:pwd
  prompt+="\001\e[0;95m\002"
  prompt+="$(whoami)"
  prompt+="\001\e[0;90m\002"
  prompt+="@"
  prompt+="\001\e[0;96m\002"
  prompt+="$(hostname)"
  prompt+="\001\e[0;90m\002"
  prompt+=':'
  prompt+="\001\e[1;94m\002"
  prompt+="$(dirs +0)"
  prompt+=' '

  # show git branch and its status if in a git tree
  local branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)

  if [[ -n "${branch}" ]]; then
    if [[ -z $(git status --short) ]]; then
      # clean -> GREEN
      prompt+="\001\e[0;32m\002"
    else
      # modified -> RED
      prompt+="\001\e[0;31m\002"
    fi 
    prompt+="${branch}"
  fi

  # change line
  prompt+="\n"

  # change the prompt to indicate the status of last executed command
  if [[ ${last_command_status} -eq 0 ]]; then
    # success -> GREEN
    prompt+="\001\e[1;92m\002"
  else
    # error -> RED
    prompt+="\001\e[1;91m\002"
  fi

  # use appropriate prompt to reflect effective uid
  if [[ $(id -u) -eq 0 ]]; then
    # root
    prompt+="Λ"
  else
    # non-root
    prompt+="λ"
  fi

  # change line + reset colors
  prompt+="\001\e[0m\002 "

  printf "${prompt}"
}

PS1='`show_bash_prompt`'

# fzf
source /usr/share/fzf/shell/key-bindings.bash

