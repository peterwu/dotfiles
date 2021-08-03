#!/usr/bin/env bash

cmd=(
  env GTK_THEME=Adwaita:dark
  yad --center --fixed --on-top
  --borders=20
  --form
  --title="Are you sure?"
  --image=computer
  --text="\tWhat do you want your computer to do?\t\n"
  --field="\t\t  ":CB
  " \tLock! \tLog Out! \tReboot! \tPower Off! \tSuspend"
  --field=" ":LBL
)

read _ choice <<< $("${cmd[@]}" | cut -d '|' -f1)

case "${choice}" in
    'Lock')
        light-locker-command --lock
        ;;
    'Log Out')
        herbstclient quit
        ;;
    'Reboot')
        systemctl reboot
        ;;
    'Power Off')
        systemctl poweroff
        ;;
    'Suspend')
        systemctl suspend
        ;;
    *)
        ;;
esac
