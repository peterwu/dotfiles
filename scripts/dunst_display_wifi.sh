#!/usr/bin/env bash

msgId="1547391577"
# icon=
ssid=$(nmcli -t -f active,ssid dev wifi|grep yes|cut -d ':' -f2)
ip4_addr=$(nmcli connection show ${ssid}|grep IP4.ADDRESS|awk '{print $2}')
msg=$(echo -e "${ssid}\n${ip4_addr}")

dunstify -a "Display Wifi" -u normal -r "$msgId" $msg
