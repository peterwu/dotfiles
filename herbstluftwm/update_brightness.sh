#!/usr/bin/env bash

# ensure to add user to the video group
# $1 : delta in brightness (0-100)

BACKLIGHT_BRIGHTNESS=/sys/class/backlight/intel_backlight/brightness

delta=$(( $1 * 12 )) # adjust to (0-1200)
brightness=$(cat $BACKLIGHT_BRIGHTNESS)

echo $(( "${brightness}" + "${delta}" )) > $BACKLIGHT_BRIGHTNESS
