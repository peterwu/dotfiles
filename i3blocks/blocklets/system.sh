#!/usr/bin/env bash

if [[ $BLOCK_BUTTON -eq 1 ]]; then
    case $1 in
      "lock" )
        /usr/local/bin/lockScreen.sh
        ;;
      "logout" )
        i3-msg exit
        ;;
      "reboot" )
        systemctl reboot
        ;;
      "shutdown")
        systemctl poweroff
        ;;
    esac
fi

exit 0
