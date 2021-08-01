#!/usr/bin/env bash

choices="\uf30d\tLock"
choices+="\n"
choices+="\uf08b\tLogout"
choices+="\n"
choices+="\uf01e\tReboot"
choices+="\n"
choices+="\uf011\tShutdown"
choices+="\n"
choices+="\uf755\tSuspend"

chosen=$(echo -e "$choices" | rofi -dmenu -i -p "Leaving herbstluftwm ... ")

case "${chosen:2}" in
    Lock)
        light-locker-command --lock
        ;;
    Logout)
        herbstclient quit
        ;;
    Reboot)
        systemctl reboot
        ;;
    Shutdown)
        systemctl poweroff
        ;;
    Suspend)
        systemctl suspend
        ;;
    *)
        ;;
esac
