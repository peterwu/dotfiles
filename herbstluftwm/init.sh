#!/usr/bin/bash

xrdb ~/.Xresources

# launch a few programs
xsetroot -cursor_name left_ptr
xset s 300 5
picom --daemon --fading --no-fading-openclose
# light-locker &

if [ -f $HOME/.fehbg ]; then
  hc spawn $HOME/.fehbg
fi

# lightdm doesn't source .xprofile
source ~/.xprofile

# Terminate already running bar instances
killall -q lemonbar

# Wait until the processes have been shut down
while pgrep -u $UID -x lemonbar >/dev/null; do sleep 1; done

~/.config/herbstluftwm/panel.sh
