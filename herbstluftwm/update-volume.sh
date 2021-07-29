#!/usr/bin/env bash

# $1: press mute button
# $2: delta in volume

delta=$2

[[ "$1" -eq 1 ]] && pamixer --toggle-mute
muted=$(pamixer --get-mute)

if [ "${delta}" -ge 0 ]; then
    pamixer --allow-boost --increase "${delta}"
else
    pamixer --allow-boost --decrease "${delta:1}"
fi

vol=$(pamixer --get-volume)

herbstclient emit_hook vol "${muted}" "${vol}"
