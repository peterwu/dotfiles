#!/usr/bin/env bash

icon=~/Pictures/lockscreen/icon.png
tmpbg=/tmp/screen.png

scrot $tmpbg
convert $tmpbg -scale 10% -scale 1000% $tmpbg
convert $tmpbg $icon -gravity center -composite -matte $tmpbg

i3lock -u -e -f -i $tmpbg
