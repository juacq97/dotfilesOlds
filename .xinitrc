#!/bin/sh

compton -b --config ~/.config/compton.conf &
xfce4-power-manager &
pcloud &
copyq &
conky -c ~/conkys/reloj-texto &
emacs --daemon &
~/.config/polybar/polybar-monitors.sh &
/usr/lib/kdeconnectd &
redshift &
xsetroot -cursor_name left_ptr &

wm=$(echo $XDG_SESSION_DESKTOP)
if [ "wm" != "bspwm" ]; then
    sxhkd -c ~/.config/sxhkd/sxhkdrc-nobspwm &
fi

