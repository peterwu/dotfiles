# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
  PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi

# User specific aliases and functions
set -o vi
alias se='sudoedit'

alias emacs='emacs -mm'
alias e='emacsclient -t -a ""'
alias vi='e'
alias vim='vi'

alias ls='ls --color' # use colors
alias la='ls -Flsa'   # list all files
alias ll='ls -Fls'    # long listing format

alias rm='rm -i'      # prompt before overwrite (but dangerous, see rm for a better approach)
alias cp='cp -i'      # prompt before overwrite (same general problem as the rm)
alias mv='mv -i'      # prompt before overwrite (same general problem as the rm)

alias bc='bc -l'

HISTSIZE=20000
HISTFILESIZE=20000

# use emacs as man pager
man() {
    local m=$@
    /usr/bin/man ${m} > /dev/null
    [[ $? -eq 0 ]] && /usr/bin/emacsclient -nw --eval "(let ((m \"${m}\")) (man m) (delete-window) t)"
}

# customize bash prompt
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
    prompt+="\001\e[0;35m\002"
    prompt+="$(date +%R)"
    prompt+=' '

    # user@host:pwd
    prompt+="\001\e[0;34m\002"
    prompt+=$USER
    prompt+="\001\e[0;38m\002"
    prompt+='@'
    prompt+="\001\e[0;38m\002"
    prompt+=$HOSTNAME
    prompt+="\001\e[0;38m\002"
    prompt+=':'
    prompt+="\001\e[1;34m\002"
    prompt+="$(dirs +0)"
    prompt+=' '

    # show git branch and its status if in a git tree
    local git_status=$(git status --branch --porcelain 2> /dev/null)
    IFS=$'\n' git_status=($git_status)

    if [[ $? -eq 0 ]]; then
      local git_branch="${git_status[0]}"
      local git_branch_regex="^##\s(\w*).*$"

      if  [[ ${git_branch} =~ ${git_branch_regex} ]]; then
        git_branch="${BASH_REMATCH[1]}"
      fi

      if [[ -n "${git_branch}" ]]; then
        if [[ ${#git_status[@]} -gt 1 ]]; then
          # modified -> RED
          prompt+="\001\e[0;31m\002"
        else
          # clean -> GREEN
          prompt+="\001\e[0;32m\002"
        fi

        prompt+="\uf126 ${git_branch}" # git icon
      fi
    fi

    # change line
    prompt+="\n"

    # change the prompt to indicate the status of last executed command
    if [[ ${last_command_status} -eq 0 ]]; then
      # success -> GREEN
      prompt+="\001\e[1;32m\002"
    else
      # error -> RED
      prompt+="\001\e[1;31m\002"
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
