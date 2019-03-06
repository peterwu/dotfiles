#!/usr/bin/env bash

msgId="1547391577"
brightness=$(light -G|cut -d '.' -f1)
icon="dialog-information"

dunstify -a "Display Brightness" -u normal -i $icon -r "$msgId" "Brightness ${brightness}%" 
