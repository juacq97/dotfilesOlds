#!/bin/sh

xwallpaper --zoom "$(cat ~/.config/bg)" &
wal -sR &
xrdb  ~/.Xresources &
#emacs --daemon &
xfce4-power-manager &
clipmenud &
/usr/lib/kdeconnectd &
redshift &
#xsetroot -cursor_name left_ptr &
flameshot &
#mpDris2 &
#sxhkd -c ~/.config/sxhkd/sxhkd-no-bspwm &
picom --experiemental-backends &
dunst &

export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR="nvim"
export OPENER="mimeopen"
export TERMINAL="st"

exec bspwm

