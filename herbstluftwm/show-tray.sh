#!/usr/bin/env bash

width=300
height=50
posx=-1
posy=30

BACKLIGHT_BRIGHTNESS=/sys/class/backlight/intel_backlight/brightness
PATH_AC="/sys/class/power_supply/AC"
PATH_BATTERY_0="/sys/class/power_supply/BAT0"
PATH_BATTERY_1="/sys/class/power_supply/BAT1"

print_batt() {
    local ac=0
    local battery_level_0=0
    local battery_level_1=0
    local battery_max_0=0
    local battery_max_1=0

    if [ -f "${PATH_AC}/online" ]; then
        ac=$(cat "${PATH_AC}/online")
    fi

    if [ -f "${PATH_BATTERY_0}/energy_now" ]; then
        battery_level_0=$(cat "${PATH_BATTERY_0}/energy_now")
    fi

    if [ -f "${PATH_BATTERY_0}/energy_full" ]; then
        battery_max_0=$(cat "${PATH_BATTERY_0}/energy_full")
    fi

    if [ -f "${PATH_BATTERY_1}/energy_now" ]; then
        battery_level_1=$(cat "${PATH_BATTERY_1}/energy_now")
    fi

    if [ -f "${PATH_BATTERY_1}/energy_full" ]; then
        battery_max_1=$(cat "${PATH_BATTERY_1}/energy_full")
    fi

    local battery_level=$((${battery_level_0} + ${battery_level_1}))
    local battery_max=$((${battery_max_0} + ${battery_max_1}))
    local battery_percent=$((${battery_level} * 100 / ${battery_max}))

    echo -e "${ac} ${battery_percent}"
}

format_batt() {
    local ac=$1
    local cap=$2
    local batt

    if [ "${ac}" -eq 1 ]; then
        icon+="\uf376" # charging
    else
        if [ "${cap}" -ge 95 ]; then
            icon+="\uf240" # full
        elif [ "${cap}" -ge 75 ]; then
            icon+="\uf241" # 3/4
        elif [ "${cap}" -ge 50 ]; then
            icon+="\uf242" # 1/2
        elif [ "${cap}" -ge 25 ]; then
            icon+="\uf243" # 1/4
        else
            icon+="\uf244" # empty
        fi
    fi

    batt="${icon}  ${cap}%"

    if [[ "${ac}" -eq 0 && "${cap}" -le 9 ]]; then
        batt="<span foreground='#ff0000'>${batt}</span>"
    fi

    if [[ "${ac}" -eq 1 && "${cap}" -ge 95 ]]; then
        batt="<span foreground='#00ff00'>${batt}</span>"
    fi

    echo -e "${batt}"
}

format_batt_details() {
    local ac=0
    local battery_level_0=0
    local battery_level_1=0
    local battery_max_0=0
    local battery_max_1=0

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
        charging_status=""
    else
        charging_status=""
    fi

    local result=(
        BAT0
        $battery_percent_0
        $charging_status
        BAT1
        $battery_percent_1
        $charging_status
    )

    echo "${result[@]}"
}

confirm_action() {
    # $1 : text for the action
    # $2 : real action to carry out

    local cmd=(
        env GTK_THEME=Adwaita:dark
        yad --center --on-top --fixed
        --title="Are you sure?"
        --image=dialog-question
        --text="\tAre you sure you want to $1 your computer now?\t"
    )

    "${cmd[@]}" && $2
}
export -f confirm_action


brightness=$(( $(cat $BACKLIGHT_BRIGHTNESS) / 12 ))
vol=$(pamixer --get-volume)
essid=$(iwctl station wlan0 show|grep 'Connected network'|awk '{print $3}')
batt=$(format_batt $(print_batt))
batt_details=($(format_batt_details))
bat0_details=$(printf "\t🞂 %7s <tt>%11d%%</tt> %13s " ${batt_details[@]:0:3})
bat1_details=$(printf "\t🞂 %7s <tt>%11d%%</tt> %13s " ${batt_details[@]:3:3})

cmd=(
    env GTK_THEME=Adwaita:dark
    yad --form
    --align-buttons
    --field="  ":SCL "${brightness}"
    --field="  ":SCL "${vol}"
    --field="":LBL ""
    --field="    ${essid}":LBL ""
    --field="  ${batt}":LBL ""
    --field="  ${bat0_details}":LBL ""
    --field="  ${bat1_details}":LBL ""
    --field="":LBL ""
    --field="   Lock":BTN      "bash -c 'confirm_action \"lock\"      \"light-locker-command --lock\"'"
    --field="   Log Out":BTN   "bash -c 'confirm_action \"log out\"   \"herbstclient quit\"'"
    --field="   Reboot":BTN    "bash -c 'confirm_action \"reboot\"    \"systemctl reboot\"'"
    --field="   Power Off":BTN "bash -c 'confirm_action \"power off\" \"systemctl poweroff\"'"
    --field="   Suspend":BTN   "bash -c 'confirm_action \"suspend\"   \"systemctl suspend\"'"
    --no-buttons
    --on-top
    --close-on-unfocus
    --fixed
    --posx=$posx
    --posy=$posy
)

"${cmd[@]}"
