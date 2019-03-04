#!/usr/bin/env bash

msgId="1547391577"
# icon=
msg=$(date)

dunstify -a "Display Battery" -u normal -r "$msgId" "$msg"
