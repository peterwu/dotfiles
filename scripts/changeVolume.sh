#!/usr/bin/env bash

msgId="1547391577"
volume=$(pamixer --get-volume)
mute=$(pamixer --get-mute)

# volume
if [[ $volume -gt 70 ]]; then
    icon="audio-volume-high"
elif [[ $volume -gt 40 ]]; then
    icon="audio-volume-medium"
elif [[ $volume -ge 0 ]]; then
    icon="audio-volume-low"
fi

# mute
if [[ $mute == "true" ]]; then
    icon="audio-volume-muted"
fi


if [[ $mute == "true" ]]; then
    # Show the sound muted notification
    dunstify -a "changeVolume" -u normal -i $icon -r "$msgId" "Volume MUTED" 
else
    # Show the volume notification
    dunstify -a "changeVolume" -u normal -i $icon -r "$msgId" "Volume ${volume}%" 
fi

# Play the volume changed sound
canberra-gtk-play -i audio-volume-change -d "changeVolume"

