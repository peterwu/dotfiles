#!/usr/bin/env bash

chosen=$(echo -e "󰌾\tLock\n󰍃\tLogout\n󰜉\tReboot\n󰐥\tShutdown\n󰤄\tSuspend\n󰒲\tHibernate" | rofi -dmenu -i -p "Leaving herbstluftwm ... ")

if [[ $chosen =~ "Lock" ]]; then
    light-locker-command --lock
elif [[ $chosen =~ "Logout" ]]; then
    herbstclient quit
elif [[ $chosen =~ "Reboot" ]]; then
    systemctl reboot
elif [[ $chosen =~ "Shutdown" ]]; then
    systemctl poweroff
elif [[ $chosen =~ "Suspend" ]]; then
    systemctl suspend
elif [[ $chosen =~ "Hibernate" ]]; then
    systemctl hibernate
fi
