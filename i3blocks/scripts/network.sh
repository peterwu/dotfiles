#!/bin/bash

networks=$(nmcli -t d | awk -F: '{if ($3=="connected") print $2}')

result=""
for network in $networks; do
  if [[ $network == "ethernet" ]]; then
    result="$result \uf796"
  elif [[ $network == "wifi" ]]; then
    result="$result \uf1eb"
  else
    result="$result \uf6ff"
  fi
done

echo -e $result | awk '{$1=$1;print}'

exit 0