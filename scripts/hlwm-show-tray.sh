#!/usr/bin/env bash

screen_width=1920
width=300
height=50
posx=$(( $screen_width - $width - 37 ))
posy=30

BACKLIGHT_BRIGHTNESS=/sys/class/backlight/intel_backlight/brightness
PATH_AC="/sys/class/power_supply/AC"
PATH_BATTERY_0="/sys/class/power_supply/BAT0"
PATH_BATTERY_1="/sys/class/power_supply/BAT1"

format_batx() {
    local battery_percent=$2
    local ac=$3

    local icon

    if [ "${ac}" -eq 1 ]; then
        icon+="" # charging
    else
        if [ "${battery_percent}" -ge 95 ]; then
            icon+="" # full
        elif [ "${battery_percent}" -ge 75 ]; then
            icon+="" # 3/4
        elif [ "${battery_percent}" -ge 50 ]; then
            icon+="" # 1/2
        elif [ "${battery_percent}" -ge 25 ]; then
            icon+="" # 1/4
        else
            icon+="" # empty
        fi
    fi

    local batt="${icon}  ${battery_percent}%"

    if [[ "${ac}" -eq 0 && "${battery_percent}" -le 9 ]]; then
        batt="<span foreground='#ff0000'>${batt}</span>"
    fi

    if [[ "${ac}" -eq 1 && "${battery_percent}" -ge 95 ]]; then
        batt="<span foreground='#00ff00'>${batt}</span>"
    fi

    echo -e "${batt}"
}

get_batt_details() {
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

    local battery_percent_0=$(("$battery_level_0 * 100 / $battery_max_0"))
    local battery_percent_1=$(("$battery_level_1 * 100 / $battery_max_1"))

    local battery_level=$(( ${battery_level_0} + ${battery_level_1} ))
    local battery_max=$(( ${battery_max_0} + ${battery_max_1} ))
    local battery_percent=$(( ${battery_level} * 100 / ${battery_max} ))

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
        BATx
        $battery_percent
        $ac
    )

    echo "${result[@]}"
}

confirm_action() {
    # $1 : text for the action
    # $2 : real action to carry out

    local cmd=(
        env GTK_THEME=Adwaita:dark
        yad --center --on-top --fixed
        --borders=20
        --form
        --title="Are you sure?"
        --image=dialog-question
        --text="\tAre you sure you want to $1 your computer now?\t"
        --field=" ":LBL
    )

    "${cmd[@]}" && $2
}
export -f confirm_action


brightness=$(( $(cat $BACKLIGHT_BRIGHTNESS) / 12 ))
vol=$(pamixer --get-volume)
essid=$(iwctl station wlan0 show|grep 'Connected network'|awk '{print $3}')
batt_details=($(get_batt_details))
bat0_details=$(printf "\t🞂 %7s <tt>%11d%%</tt> %13s " ${batt_details[@]:0:3})
bat1_details=$(printf "\t🞂 %7s <tt>%11d%%</tt> %13s " ${batt_details[@]:3:3})
batx_details=$(format_batx ${batt_details[@]:6:3})

cmd=(
    env GTK_THEME=Adwaita:dark
    yad --form
    --align-buttons
    --field="  ":SCL "${brightness}"
    --field="  ":SCL "${vol}"
    --field="":LBL ""
    --field="    ${essid}":LBL ""
    --field="  ${batx_details}":LBL ""
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
