xrdb -merge ~/.Xresources

#exec jwm -f ~/.config/jwm/jwmrc

export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
ibus-daemon -drx

feh --no-fehbg --bg-scale /usr/share/backgrounds/default.png &

exec xterm -class termbar -e ~/.config/cwm/termbar &

exec cwm -c ~/.config/cwm/cwmrc
