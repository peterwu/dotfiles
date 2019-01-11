#!/bin/bash

networks=$(nmcli -t d | awk -F: '{if ($3=="connected" && ($2=="ethernet" || $2=="wifi")) print $2}')

result=""
for network in $networks; do
  if [[ $network == "ethernet" ]]; then
    result="$result \uf6ff"
  else
    result="$result \uf1eb"
  fi
done

echo -e $result | awk '{$1=$1;print}'

exit 0
