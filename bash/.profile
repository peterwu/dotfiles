# nnn
export NNN_BMS='d:~/Documents;j:~/Downloads;p:~/Projects'
export NNN_COLORS='4150'
export NNN_PLUG='f:fzcd;'

# fzf
export FZF_DEFAULT_COMMAND='find .'
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --multi --color=light
                         --preview "(highlight -O ansi -l {} || cat {} || tree -C {}) 2>/dev/null | head -100"'
export FZF_ALT_C_OPTS='--preview "tree -C {} | head -100"'

# ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

ibus-daemon -drx
