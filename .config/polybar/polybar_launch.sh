#!/bin/bash

#MATA POLYBAR Y LO VUELVE A CORRER. SE USA EN i3 CONFIG.

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location 
#~/.i3/polybar/config

polybar -c ~/.config/polybar/config example -r &

