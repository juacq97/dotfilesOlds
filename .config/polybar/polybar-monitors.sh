#! /bin/sh

#MATA POLYBAR Y LO VUELVE A CORRER. SE USA EN i3 CONFIG.

# Terminate already running bar instances
if [ $(pgrep polybar) ]; then
    killall polybar
fi


for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar -c ~/.config/polybar/config example -r &
done
    
