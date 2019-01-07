#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias ll="ls -al"
alias emacs='emacsclient -t -a emacs'

set -o vi

source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
source <(kitty + complete setup bash)
source <(kubectl completion bash)

### This Changes The PS1 ### 
PROMPT_COMMAND=__prompt_command # Func to gen PS1 after CMDs

function __prompt_command {
  local EXIT="$?"

  # Dracula Colors
  local BLACK="\[$(tput setaf 0)\]"
  local RED="\[$(tput setaf 1)\]"
  local GREEN="\[$(tput setaf 2)\]"
  local YELLOW="\[$(tput setaf 3)\]"
  local BLUE="\[$(tput setaf 4)\]"
  local PURPLE="\[$(tput setaf 5)\]"
  local CYAN="\[$(tput setaf 6)\]"
  local WHITE="\[$(tput setaf 7)\]"

  # Clear attributes
  local RESET="\[$(tput sgr0)\]"

  # Title bar - "user@host: ~"
  function titlebar {
    local title="\u@\h: \W"
    local titlebar="\[\033]0;"$title"\007\]"
    echo $titlebar
  }

  # Git branch
  function git_branch {
    if [[  -d .git ]]; then
      local status=$(git_status)
      local branch=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)\ /');
      echo $status$branch
    fi
  }

  # Git status
  function git_status {
    [[ `git status --porcelain` ]] && echo ${RED} || echo ${GREEN}
  }

  function arrow {
    # ↬ : U+21AC
    # → : U+2192
    local arrow="→ "
    if [ $EXIT != 0 ]; then
      arrow="${RED}${arrow}${RESET}"      # Add red if exit code non 0
    else
      arrow="${GREEN}${arrow}${RESET}"
    fi
  
    echo $arrow
  }
  
  function prompt {
    local prompt="\$ "
    [[ $(id -u) == "0"  ]] && prompt="${RED}${prompt}"
    prompt="${prompt}${RESET}"
    echo $prompt
  }

  PS1="$(titlebar)$(arrow)${BLUE}\w $(git_branch)${RESET}\n$(prompt)"
}

