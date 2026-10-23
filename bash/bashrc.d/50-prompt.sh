# Customize bash prompt
_parse_git_branch() {
    # show git branch and its status if in a git tree
    local result=''
    local git_result=$(git status --branch --porcelain=2 2> /dev/null) || return

    local git_branch=$(printf "%s\n" "${git_result[@]}" | grep '^# branch.head' | cut -d' ' -f3)
    local git_status=$(printf "%s\n" "${git_result[@]}" | grep -v '^# ')

    if [[ -n "${git_branch}" ]]; then
        if [[ -n "${git_status}" ]]; then
            # modified -> RED
            result+="\001$(tput sgr0; tput setaf 1)\002"
        else
            # clean -> GREEN
            result+="\001$(tput sgr0; tput setaf 2)\002"
        fi

        result+="* ${git_branch}"
    fi

    echo -ne "${result}"
}

_show_last_command_status() {
    local result=''

    # change the prompt to indicate the status of last executed command
    if [[ ${last_command_status} -eq 0 ]]; then
        # success -> GREEN
        result+="\001$(tput sgr0; tput setaf 2)\002"
    else
        # error -> RED
        result+="\001$(tput sgr0; tput setaf 1)\002"
    fi

    echo -ne "${result}"
}

_customize_shell_prompt() {
    # The entire table of ANSI color codes
    # https://gist.github.com/iamnewton/8754917
    # \### format must be used in functions
    # \001 == \[
    # \002 == \]
    # \033 == \e

    local prompt='\n'

    # current time
    prompt+="\001$(tput sgr0; tput setaf 5)\002"
    prompt+='\D{%F %R}'
    prompt+=' '

    # user@host:pwd
    prompt+="\001$(tput sgr0; tput setaf 4)\002"
    prompt+='\u'
    prompt+="\001$(tput sgr0; tput setaf 0)\002"
    prompt+='@'
    prompt+='\h'
    prompt+=':'
    prompt+="\001$(tput sgr0; tput setaf 4)\002"
    prompt+='\w'
    prompt+=' '

    # show git branch and its status if in a git tree
    prompt+='$(_parse_git_branch)'
    prompt+=' '

    # show python virtual environment
    prompt+="\001$(tput sgr0; tput setaf 8)\002"
    prompt+="${VIRTUAL_ENV:+($(basename "${VIRTUAL_ENV}"))}"

    # change line
    prompt+='\n'

    # change the prompt to indicate the status of last executed command
    prompt+='$(_show_last_command_status)'
    prompt+='\$ '

    # reset colors
    prompt+="\001$(tput sgr0)\002"

    PS1="${prompt}"
}

PROMPT_COMMAND='last_command_status=$?; _customize_shell_prompt'
