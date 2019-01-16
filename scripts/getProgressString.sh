#!/usr/bin/env bash
# getProgressString <TOTAL ITEMS> <FILLED LOOK> <NOT FILLED LOOK> <STATUS>
# For instance:
# $ getProgressString 10 "#" "-" 50
# #####-----
# Note: if you want to use | in your progress bar string you need to change the delimiter in the sed commands

items="$1" # The total number of items(the width of the bar)
filled_item="$2" # The look of a filled item 
not_filled_item="$3" # The look of a not filled item
status="$4" # The current progress status in percent

# calculate how many items need to be filled and not filled
filled_items=$(echo "((${items} * ${status})/100 + 0.5) / 1" | bc)
not_filled_items=$(echo "$items - $filled_items" | bc)

# Assemble the bar string
msg=$(printf "%${filled_items}s" | sed "s| |${filled_item}|g")
msg=${msg}$(printf "%${not_filled_items}s" | sed "s| |${not_filled_item}|g")
echo "$msg"
