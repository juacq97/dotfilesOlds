#!/bin/sh

emacs --daemon &
#compton -b --config ~/.config/compton.conf &
picom -b --config ~/.config/compton.conf &
#anicom -b --config ~/.config/anicom.conf &
xfce4-power-manager &
#pcloud &
copyq &
#conky -c ~/conkys/reloj-texto &
#~/.config/polybar/polybar-monitors.sh &
/usr/lib/kdeconnectd &
/lib/x86_64-linux-gnu/libexec/kdeconnectd &
dunst & 
redshift &
xsetroot -cursor_name left_ptr &
flameshot &
mpDris2 &
volumeicon &


export QT_QPA_PLATFORMTHEME="qt5ct"

#
exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) bspwm
#exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) startfluxbox
#exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) openbox-session

