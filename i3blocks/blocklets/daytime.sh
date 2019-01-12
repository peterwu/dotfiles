#!/bin/bash

scaling=2
daytime=$(date "+%F %a %R %Z")
daytime=${daytime^^}
min_width=$(xdpyinfo | awk '/dimensions/{print $2}' | cut -d 'x' -f1)
min_width=$((min_width/scaling))

echo -e "{\"full_text\":\"$daytime\", \"min_width\":\"$min_width\"}"

exit 0
