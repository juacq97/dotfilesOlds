#!/bin/sh

emacs --daemon &
xfce4-power-manager &
clipit -n &
/usr/lib/libexec/kdeconnectd &
redshift &
xsetroot -cursor_name left_ptr &
flameshot &
mpDris2 &
xrdb -load .Xresources &
nitrogen --restore &
#sxhkd -c ~/.config/sxhkd/sxhkd-no-bspwm &
picom --experimental-backends &

export QT_QPA_PLATFORMTHEME="qt5ct"

exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) bspwm
#exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) startfluxbox
#exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) openbox-session

