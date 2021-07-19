#!/usr/bin/env bash

# dependencies
# - inotify
# - yad

HLWM_HOME=~/.config/herbstluftwm
SEPARATOR=" "

monitor="${1:-0}"

# helpers
uniq_linebuffered() {
    awk '$0 != l {print; l=$0; fflush();}' "$@"
}

# printers
print_date() {
    date +$'date\t%H:%M'
}

print_vol() {
    # pamixer --get-volume
    # pamixer --get-mute
    # format: vol/mute?/vol
    echo -e "vol\t0\t45"
}

print_net() {
    # /proc/net/wireless
    # format: net/eth?/strenth
    strength=$(awk 'NR==3 {printf "%d",$3}' /proc/net/wireless)

    # hard code eth=0 for now
    echo -e "net\t0\t$strength"
}

print_batt() {
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

    battery_level=$(("$battery_level_0 + $battery_level_1"))
    battery_max=$(("$battery_max_0 + $battery_max_1"))

    battery_percent=$(("$battery_level * 100"))
    battery_percent=$(("$battery_percent / $battery_max"))

    echo -e "batt\t$ac\t$battery_percent"
}

# formatters
format_focused_tag() {
    echo -n "%{+o}%{F#ffffff}$1%{F-}%{-o}"
}

format_occupied_tag() {
    echo -n "%{F#ffffff}$1%{F-}"
}

format_unoccupied_tag() {
    echo -n "%{F#505050}$1%{F-}"
}

format_tags() {
    tags=$1
    ws=""

    for i in "${tags[@]}"; do
        tag_name="${i:1}"
        tag_status="${i:0:1}"

        ws+="%{A1:herbstclient use ${tag_name}:}"

        case ${tag_status} in
            ".") # unoccupied
                ws+=$(format_unoccupied_tag "${tag_name}")
                ;;
            ":") # occupied
                ws+=$(format_occupied_tag "${tag_name}")
                ;;
            "+") # unfocused
                ws+="${tag_name}"
                ;;
            "#") # focused
                ws+=$(format_focused_tag "${tag_name}")
                ;;
            *) # NONE
                ;;
        esac
        ws+="%{A}"
        ws+=$SEPARATOR
    done

    echo "$ws"
}

format_date() {
    now="$@"

    now="%{A1:herbstclient spawn $HLWM_HOME/show_time.sh:}$now%{A}"

    echo -e "$now"
}

format_vol() {
    mute=$1
    vol=$2

    if [ "$mute" -eq 1 ]; then
        icon="\uf6a9"
    else
        if [ "$vol" -gt 66 ]; then
            icon="\uf028"
        elif [ "$vol" -gt 33 ]; then
            icon="\uf6a8"
        elif [ "$vol" -gt 0 ]; then
            icon="\uf027"
        else
            icon="\uf026"
        fi
    fi

    echo -e "$icon"
}

format_net() {
    eth=$1
    strength=$((${2:-0} * 100 / 70))

    if [ "$eth" -eq 1 ]; then
        icon="\uf6ff"
    else
        if [ "$strength" -gt 66 ]; then
            icon="\uf1eb"
        elif [ "$strength" -gt 33 ]; then
            icon="\uf6ab"
        else
            icon="\uf6aa"
        fi
    fi

    echo -e "$icon"
}

format_batt() {
    ac=$1
    cap=$2

    if [ "$ac" -eq 1 ]; then
        icon+="\uf376" # power plug
    else
        if [ "$cap" -gt 95 ]; then
            icon+="\uf240" # full
        elif [ "$cap" -gt 75 ]; then
            icon+="\uf241" # 3/4
        elif [ "$cap" -gt 50 ]; then
            icon+="\uf242" # 1/2
        elif [ "$cap" -gt 25 ]; then
            icon+="\uf243" # 1/4
        else
            icon+="\uf244" # empty
        fi
    fi

    if [[ "$ac" -eq 0 && "$cap" -le 9 ]]; then
        batt="%{F#ff0000}$icon $cap%%%{F-}"
    elif [[ "$ac" -eq 1 && "$cap" -gt 95 ]]; then
        batt="%{F#00ff00}$icon%{F-}"
    else
        batt="$icon"
    fi

    echo -e "$batt"
}

format_tray() {
    tray=$1

    tray="%{A1:herbstclient spawn $HLWM_HOME/show_tray.sh:}$tray%{A}"

    echo -e "$tray"
}

# the fun begins
{
    # volume
    print_vol

    # time
    while :; do
        print_date
        sleep 1 || break
    done > >(uniq_linebuffered) &
    pids[0]=$!

    # battery info
    print_batt
    inotifywait -m -q \
                /sys/class/power_supply/AC/uevent \
                /sys/class/power_supply/BAT{0,1}/capacity \
                -e close | \
        while read file event; do
            print_batt
        done > >(uniq_linebuffered) &
    pids[1]=$!

    # net
    while :; do
        print_net
        sleep 30 || break
    done > >(uniq_linebuffered) &
    pids[2]=$!

    # wait for events
    herbstclient --idle

    # clean up
    for pid in "${pids[@]}"; do
        kill $pid
    done
} 2> /dev/null | {
    IFS=$'\t' read -ra tags <<< "$(herbstclient tag_status $monitor)"
    ws="$(format_tags $tags)"

    while :; do
        IFS=$'\t' read -ra cmd || break
        case "${cmd[0]}" in
            tag*)
                IFS=$'\t' read -ra tags <<< "$(herbstclient tag_status $monitor)"
                ws=$(format_tags $tags)
                ;;
            date)
                date=$(format_date ${cmd[@]:1})
                ;;
            vol)
                vol=$(format_vol ${cmd[@]:1})
                ;;
            net)
                net=$(format_net ${cmd[@]:1})
                ;;
            batt)
                batt=$(format_batt ${cmd[@]:1})
                ;;
            reload)
                exit
                ;;
            quit_panel)
                exit
                ;;
            *)
                ;;
        esac

        # print the final result
        LEFT="${ws}"
        CENTER="${date}"
        RIGHT=$(format_tray "${vol}${SEPARATOR}${net}${SEPARATOR}${batt}")

        echo -e "${SEPARATOR}%{l}${LEFT}%{c}${CENTER}%{r}${RIGHT}${SEPARATOR}"
    done
} 2> /dev/null | lemonbar -p -a 15 \
                          -g "1920x30+0+0" \
                          -f "Iosevka Fusion:size=11" \
                          -f "Font Awesome 5 Pro:size=11" \
                          -B "#000000" -F "#ffffff" | "$SHELL"
