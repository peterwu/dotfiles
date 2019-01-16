#!/usr/bin/env bash

icon=/usr/share/icons/Adwaita/512x512/emblems/emblem-readonly.png
tmpbg=/tmp/screen.png

scrot $tmpbg
convert $tmpbg -scale 10% -scale 1000% $tmpbg
convert $tmpbg $icon -gravity center -composite -matte $tmpbg

i3lock -n -u -e -f -i $tmpbg
