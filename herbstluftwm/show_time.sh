#!/usr/bin/env bash

# get time for different time zones
# $ TZ=Asia/Singapore date
# $ zdump

# xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/'

screen_width=1920
width=360

# to the right most position
posx=-1
posy=30

# to the center
posx=$(("($screen_width - $width) / 2"))
posx=780

GTK_THEME=Adwaita:dark      \
         yad --calendar     \
         --show-weeks       \
         --no-buttons       \
         --close-on-unfocus \
         --width=$width     \
         --posx=$posx       \
         --posy=$posy
