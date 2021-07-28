#!/usr/bin/bash

xrdb ~/.Xresources

# launch a few programs
xsetroot -cursor_name left_ptr
xset s 300 5
light-locker --lock-on-suspend --lock-on-lid --idle-hint &

picom --daemon --fading --no-fading-openclose
feh --no-fehbg --bg-scale ~/Pictures/Wallpapers/default

# lightdm doesn't source .xprofile
source ~/.xprofile

# Terminate already running bar instances
killall -q lemonbar

# Wait until the processes have been shut down
while pgrep -u $UID -x lemonbar >/dev/null; do sleep 1; done

# do multi monitor setup here, e.g.:
# herbstclient set_monitors 1280x1024+0+0 1280x1024+1280+0
# or simply:
herbstclient detect_monitors

# find the panel
panel=~/.config/herbstluftwm/panel.sh
[ -x "$panel" ] || panel=/etc/xdg/herbstluftwm/panel.sh
for monitor in $(herbstclient list_monitors | cut -d: -f1) ; do
    # start it on each monitor
    "$panel" "$monitor" &
done
