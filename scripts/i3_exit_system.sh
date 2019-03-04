#!/usr/bin/env bash

chosen=$(echo -e "[Cancel]\nLock\nLogout\nReboot\nShutdown\nSuspend\nHibernate" | rofi -dmenu -i -p "Exiting i3 ... ")

if [[ $chosen = "Lock" ]]; then
	~/.config/scripts/i3_lock_screen.sh
elif [[ $chosen = "Logout" ]]; then
	i3-msg exit
elif [[ $chosen = "Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = "Suspend" ]]; then
	systemctl suspend
elif [[ $chosen = "Hibernate" ]]; then
	systemctl hibernate
fi
