#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Launch bar

external_monitor=$(autorandr | grep detected | cut -f1 -d' ' | tr -d ' ')
case $external_monitor in
    dock) polybar external & polybar laptop &;;
    external) polybar external -r &;;
    laptop) polybar laptop -r &;;
esac
