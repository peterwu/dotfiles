#!/usr/bin/env bash

key=$(($RANDOM * 100))

screen_width=$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/' | cut -dx -f1)
width=360
height=400
posy=30
posx=$((($screen_width - $width) / 2))

time_zones=(
    America/Toronto
    America/Vancouver
    Asia/Shanghai
)

get_tz_data() {
    for tz in "${time_zones[@]}"; do
        data+=($(env TZ="${tz}" date +"${tz##*/} %H:%M <tt>%:::z</tt>"))
    done

    number_of_zones=$(( ${#data[@]} / ${#time_zones[@]} ))
    for ((i=0; i < $number_of_zones; i++)); do
        for ((j=0; j < 3; j++)); do
            result+=("${data[i+3*j]}")
        done
    done

    echo "${result[@]}"
}

format_world_clock_cmd() {
    c=($@)

    for e in $(get_tz_data); do
        f="--field=$e:LBL"
        c+=("${f}")
    done

    echo "${c[@]}"
}

cmd=(
    env GTK_THEME=Adwaita:dark
    yad --plug=$key --tabnum=1
    --fixed
    --calendar --show-weeks
)

"${cmd[@]}" &

cmd=(
    env GTK_THEME=Adwaita:dark
    yad --plug=$key --tabnum=2
    --form
    --text="<b>Clocks</b>"
    --columns=3
    --align=right
)

cmd=($(format_world_clock_cmd ${cmd[@]}))

"${cmd[@]}" &

cmd=(
    env GTK_THEME=Adwaita:dark
    yad --paned --key=$key
    --text="$(date +'%A\n<b><big>%B %_d %Y</big></b>')"
    --close-on-unfocus
    --no-buttons
    --on-top
    --posx=$posx
    --posy=$posy
    --width=$width
    --height=$height
)

"${cmd[@]}" &
