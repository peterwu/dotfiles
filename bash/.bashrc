# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi

# Don't put duplicate lines or lines starting with space in the history.
# See `man bash` for more options.
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it.
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in `man bash`.
HISTSIZE=20000
HISTFILESIZE=20000

# Default editor
if pgrep --exact emacs > /dev/null; then
    export EDITOR="/usr/bin/emacsclient --tty"
elif [ -x /usr/bin/nvim ]; then
    export EDITOR="/usr/bin/nvim"
fi

# User specific aliases and functions
set -o vi

if pgrep --exact emacs > /dev/null; then
    alias emacs="emacs --maximized"
    alias e="emacsclient --tty"
    alias magit="emacsclient --tty --eval '(magit-status)'"
fi

if [ -x /usr/bin/nvim ]; then
    alias vi="vim"
    alias vim="nvim"
    alias vimdiff="nvim -d"
fi

alias E="sudo --edit"

alias ls="ls --color=auto --group-directories-first --indicator-style=slash -v"
alias ll="ls -l --classify --size"
alias la="ls -l --classify --size --all"

alias rm="rm --interactive"
alias cp="cp --interactive"
alias mv="mv --interactive"

alias bc="bc --mathlib"

alias diff="diff --color=auto"

alias dir="dir --color=auto"
alias vdir="vdir --color=auto"

alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"

alias rsync="rsync --progress"

# Colourize man pages
man() {
    local cmd=(
        env
        LESS_TERMCAP_mb=$(tput bold; tput setaf 6)
        LESS_TERMCAP_md=$(tput bold; tput setaf 6)
        LESS_TERMCAP_me=$(tput sgr0)
        LESS_TERMCAP_se=$(tput rmso; tput sgr0)
        LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
        LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 4)
        LESS_TERMCAP_mr=$(tput rev)
        LESS_TERMCAP_mh=$(tput dim)
        LESS_TERMCAP_ZN=$(tput ssubm)
        LESS_TERMCAP_ZV=$(tput rsubm)
        LESS_TERMCAP_ZO=$(tput ssupm)
        LESS_TERMCAP_ZW=$(tput rsupm)
        man "$@"
    )

    "${cmd[@]}"
}

# Customize bash prompt
show_bash_prompt() {
    # The entire table of ANSI color codes
    # https://gist.github.com/iamnewton/8754917
    # \### format must be used in functions
    # \001 == \[
    # \002 == \]
    # \033 == \e

    local last_command_status=$?
    local prompt="\n"

    # begin box drawing
    prompt+="\001"
    prompt+=$(tput sgr0)
    prompt+="\002"
    prompt+="╭ "

    # current time
    prompt+="\001"
    prompt+=$(tput sgr0; tput setaf 5)
    prompt+="\002"
    prompt+=$(date +%R)
    prompt+=" "

    # user@host:pwd
    prompt+="\001"
    prompt+=$(tput sgr0; tput setaf 4)
    prompt+="\002"
    prompt+=$USER
    prompt+="\001"
    prompt+=$(tput sgr0; tput setaf 0)
    prompt+="\002"
    prompt+="@"
    prompt+=$HOSTNAME
    prompt+=":"
    prompt+="\001"
    prompt+=$(tput sgr0; tput bold; tput setaf 4)
    prompt+="\002"
    prompt+=$(dirs +0)
    prompt+=" "

    # show git branch and its status if in a git tree
    local git_status=$(git status --branch --porcelain 2> /dev/null)
    IFS=$'\n' git_status=($git_status)

    if [ $? -eq 0 ]; then
        local git_branch="${git_status[0]}"
        local git_branch_regex="^##\s(\w*).*$"

        if  [[ ${git_branch} =~ ${git_branch_regex} ]]; then
            git_branch="${BASH_REMATCH[1]}"
        fi

        if [ -n "${git_branch}" ]; then
            if [[ ${#git_status[@]} -gt 1 ]]; then
                # modified -> RED
                prompt+="\001"
                prompt+=$(tput sgr0; tput setaf 1)
                prompt+="\002"
            else
                # clean -> GREEN
                prompt+="\001"
                prompt+=$(tput sgr0; tput setaf 2)
                prompt+="\002"
            fi

            prompt+=" ${git_branch}" # git icon
        fi
    fi

    # change line
    prompt+="\n"

    # end box drawing
    prompt+="\001"
    prompt+=$(tput sgr0)
    prompt+="\002"
    prompt+="╰ "

    # change the prompt to indicate the status of last executed command
    if [ ${last_command_status} -eq 0 ]; then
        # success -> GREEN
        prompt+="\001"
        prompt+=$(tput sgr0; tput bold; tput setaf 2)
        prompt+="\002"
    else
        # error -> RED
        prompt+="\001"
        prompt+=$(tput sgr0; tput bold; tput setaf 1)
        prompt+="\002"
    fi

    # use appropriate prompt to reflect effective uid
    if [ $(id -u) -eq 0 ]; then
        # root
        prompt+="Λ"
    else
        # non-root
        prompt+="λ"
    fi

    # reset colors
    prompt+="\001"
    prompt+=$(tput sgr0)
    prompt+="\002"
    prompt+=" "

    printf "${prompt}"
}

# no double quotes here
PS1='$(show_bash_prompt)'
