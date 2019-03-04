#!/usr/bin/env bash

msgId="1547391577"
# icon=
msg=$(acpi --battery|grep 'Battery 0'|cut -d ',' -f3|xargs)

dunstify -a "Display Battery" -u normal -r "$msgId" "Battery 0" "$msg"
