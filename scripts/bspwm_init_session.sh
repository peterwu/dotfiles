#!/usr/bin/env bash

/usr/bin/xrdb ~/.Xresources &
/usr/bin/feh --bg-scale ~/Pictures/Wallpapers/wallpaper.png &
/usr/bin/compton -b -c -f &
/usr/bin/urxvtd -q -f -o &
/usr/bin/xautolock -detectsleep -time 5 -locker /usr/bin/slock &

# autostart some programs
~/.config/scripts/bspwm_launch_polybar.sh &
