[colors]
background = ${xrdb:background}
foreground = ${xrdb:foreground}
# foreground = #fffdf6e3
# background = #ff002b36
# primary = #ffb52a
# secondary = #e60053
# alert = #bd2c40
# red = dc322f

[bar/main]
font-0 = "Roboto:size=22;4"
font-1 = "Material Design Icons:size=28;4"
font-2 = "Weather Icons:size=24;4"
font-3 = "Sarasa Gothic SC:size=20;4"

fixed-center = true
height = 42
module-margin-right = 1
modules-left = i3
modules-center = title
modules-right = wireless-network pulseaudio battery date system-exit

background = ${colors.background}
foreground = ${colors.foreground}

[module/i3]
type = internal/i3

ws-icon-0 = 1;
ws-icon-1 = 2;
ws-icon-2 = 3;獵
ws-icon-3 = 4;
ws-icon-4 = 5;
ws-icon-5 = 6;
ws-icon-default = 

format = <label-state> <label-mode>

label-focused = %icon%
label-focused-padding = 1
label-unfocused = %icon%
label-unfocused-padding = 1
label-visible = %icon%
label-visible-padding = 1
label-urgent = %icon%
label-urgent-padding = 1

label-focused-foreground = #d33682
label-separator-padding = 2

[module/title]
type = internal/xwindow

[module/weather]
type = custom/script
exec = ~/.config/scripts/polybar_display_weather
interval = 600
label-font = 3

[module/wireless-network]
type = internal/network
interface = wlp4s0

format-connected = <ramp-signal>
label-disconnected = 

ramp-signal-0 = 冷
ramp-signal-1 = 爛
ramp-signal-2 = 嵐
ramp-signal-3 = 襤
ramp-signal-4 = 蠟

[module/pulseaudio]
type = internal/pulseaudio

format-volume = <ramp-volume>
label-muted = 

ramp-volume-0 = 
ramp-volume-1 = 
ramp-volume-2 = 

[module/battery]
type = internal/battery
full-at = 99

battery = BAT0
adapter = AC

format-charging = <animation-charging> <label-charging>
format-discharging = <ramp-capacity> <label-discharging>
label-charging = %percentage%
label-discharging = %percentage%
label-full = %percentage%

ramp-capacity-0 = 
ramp-capacity-1 = 
ramp-capacity-2 = 
ramp-capacity-3 = 
ramp-capacity-4 = 
ramp-capacity-5 = 
ramp-capacity-6 = 
ramp-capacity-7 = 
ramp-capacity-8 = 
ramp-capacity-9 = 
ramp-capacity-10 = 

animation-charging-0 = 
animation-charging-1 = 
animation-charging-2 = 
animation-charging-3 = 
animation-charging-4 = 
animation-charging-5 = 
animation-charging-6 = 
animation-charging-7 = 
animation-charging-8 = 
animation-charging-9 = 
animation-charging-10 = 
; Framerate in milliseconds
animation-charging-framerate = 750

[module/date]
type = internal/date
date = %R
date-alt = %a %F %R %Z
format =  <label>

[module/system-exit]
type = custom/text
content = 
click-left = ~/.config/scripts/i3_exit_system

;;;;;;;;;;;;;;;;
; vim:ft=dosini
