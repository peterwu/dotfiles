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

# startx
#read -e -p "Do you want to startx? (Y/n) " ANSWER
#ANSWER=${ANSWER^}

#[[ ${ANSWER:=Y} == "Y" ]] && exec startx
