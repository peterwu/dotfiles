#!/usr/bin/env bash

vol=$(pamixer --get-volume)
muted=$(pamixer --get-mute)

herbstclient emit_hook vol "${muted}" "${vol}"
