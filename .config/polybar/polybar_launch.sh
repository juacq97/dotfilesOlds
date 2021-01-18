#!/bin/bash

#MATA POLYBAR Y LO VUELVE A CORRER. SE USA EN i3 CONFIG.

# Terminate already running bar instances
killall polybar

polybar -c ~/.config/polybar/config example -r &

