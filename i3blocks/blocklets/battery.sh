#!/usr/bin/env bash

threshold=5

capacity=$(cat /sys/class/power_supply/BAT0/capacity)

if [[ $capacity -gt 75 ]]; then
    icon="\uf240"
elif [[ $capacity -gt 50 ]]; then
    icon="\uf241"
elif [[ $capacity -gt 25 ]]; then
    icon="\uf242"
elif [[ $capacity -ge $threshold ]]; then
    icon="\uf243"
else
    icon="\uf244"
fi

status=$(cat /sys/class/power_supply/BAT0/status)
if [[ $status == "Charging" ]]; then
  icon="\uf1e6"
fi

# echo -e "$icon"
echo -e "$icon $capacity"


if [[ $capacity -ge $threshold ]]; then
    exit_code=0;
else
    exit_code=33;
fi

exit $exit_code
