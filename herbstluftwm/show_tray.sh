#!/usr/bin/env bash

width=290
height=120
posx=-1
posy=30

PATH_AC="/sys/class/power_supply/AC"
PATH_BATTERY_0="/sys/class/power_supply/BAT0"
PATH_BATTERY_1="/sys/class/power_supply/BAT1"

ac=0
battery_level_0=0
battery_level_1=0
battery_max_0=0
battery_max_1=0

if [ -f "$PATH_AC/online" ]; then
    ac=$(cat "$PATH_AC/online")
fi

if [ -f "$PATH_BATTERY_0/energy_now" ]; then
    battery_level_0=$(cat "$PATH_BATTERY_0/energy_now")
fi

if [ -f "$PATH_BATTERY_0/energy_full" ]; then
    battery_max_0=$(cat "$PATH_BATTERY_0/energy_full")
fi

if [ -f "$PATH_BATTERY_1/energy_now" ]; then
    battery_level_1=$(cat "$PATH_BATTERY_1/energy_now")
fi

if [ -f "$PATH_BATTERY_1/energy_full" ]; then
    battery_max_1=$(cat "$PATH_BATTERY_1/energy_full")
fi

battery_percent_0=$(("$battery_level_0 * 100 / $battery_max_0"))
battery_percent_1=$(("$battery_level_1 * 100 / $battery_max_1"))


if [ "$ac" -eq 1 ]; then
    charging_status=" +++ "
else
    charging_status=" --- "
fi

GTK_THEME=Adwaita:dark                                  \
         yad --list                                     \
         --width=$width                                 \
         --height=$height                               \
         --posx=$posx                                   \
         --posy=$posy                                   \
         --no-buttons                                   \
         --no-selection                                 \
         --no-click                                     \
         --tail                                         \
         --close-on-unfocus                             \
         --column "Battery"                             \
         --column "Capacity"                            \
         --column "Status"                              \
         BAT0 "$battery_percent_0 %" "$charging_status" \
         BAT1 "$battery_percent_1 %" "$charging_status"
