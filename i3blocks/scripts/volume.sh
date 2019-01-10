#!/bin/bash

volume=$(pamixer --get-volume)
volume=50
mute=$(pamixer --get-mute)
mute=true
mute=false

# volume
if [[ $volume -gt 50 ]]; then
  icon="\uf028"
elif [[ $volume -gt 0 ]]; then
  icon="\uf027 "
else
  icon="\uf026"
fi

# mute
if [[ $mute == 'true' ]]; then
  icon="\uf6a9"
fi

echo -e "$icon $volume"

exit 0