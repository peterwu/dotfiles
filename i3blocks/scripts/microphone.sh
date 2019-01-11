#!/bin/bash

mute=$(pamixer --source 1 --get-mute)

if [[ $mute == "true" ]]; then
    icon="\uf131"
else
    icon="\uf130"
fi

echo -e $icon

exit 0
