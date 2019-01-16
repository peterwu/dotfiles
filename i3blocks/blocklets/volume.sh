#!/usr/bin/env bash

msgId="1547391577"
volume=$(pamixer --sink 0 --get-volume)
mute=$(pamixer --sink 0 --get-mute)

# volume
if [[ $volume -gt 50 ]]; then
    icon="\uf028"
elif [[ $volume -gt 0 ]]; then
    icon="\uf027"
else
    icon="\uf026"
fi

# mute
if [[ $mute == "true" ]]; then
    icon="\uf6a9"
fi

echo -e "$icon"

if [[ $BLOCK_BUTTON -eq 3 ]]; then
    /usr/local/bin/changeVolume.sh
fi

exit 0
