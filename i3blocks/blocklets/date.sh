#!/usr/bin/env bash

msgId="1547391577"
date=$(date "+%R %Z")
date=${date^^}

if [[ $BLOCK_BUTTON -eq 3 ]]; then 
    dunstify -a "showDate" -u normal -i appointment-soon -r "$msgId" \ "$(date)"
fi

echo -e "$date"

exit 0

