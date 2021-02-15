#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar
polybar laptop &

external_monitor=$(xrandr --query | grep 'DP-2')
if [[ $external_monitor = *connected* ]]; then
    polybar external &
fi
