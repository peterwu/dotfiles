#!/bin/bash

threshold=5

# loop all the batteries installed in the system
for battery in /sys/class/power_supply/BAT?; do
    present=$(cat $battery/present)
    if [[ $present -eq 1 ]]; then
        capacity=$(cat $battery/capacity)
    fi
done

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

echo -e "$icon $capacity"


if [[ $capacity -ge $threshold ]]; then
    exit_code=0;
else
    exit_code=33;
fi

exit $exit_code
