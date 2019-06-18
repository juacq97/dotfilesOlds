#! /bin/sh

#MATA POLYBAR Y LO VUELVE A CORRER. SE USA EN i3 CONFIG.

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar -c ~/.config/polybar/config example -r &
  done
else
polybar -c ~/.config/polybar/config example -r &
fi
