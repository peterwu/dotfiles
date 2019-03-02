#!/usr/bin/env bash

chosen=$(echo -e "[Cancel]\nLock\nLogout\nReboot\nShutdown\nSuspend\nHibernate" | rofi -dmenu -i -p "Leaving bspwm ... ")

if [[ $chosen = "Lock" ]]; then
	slock
elif [[ $chosen = "Logout" ]]; then
	bspc quit
elif [[ $chosen = "Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = "Suspend" ]]; then
	systemctl suspend
elif [[ $chosen = "Hibernate" ]]; then
	systemctl hibernate
fi
