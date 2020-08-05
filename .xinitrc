#! /bin/sh

xrandr --output DP-2 --auto --same-as LVDS-1 &
nitrogen --restore &
#wal -sR &
xrdb  ~/.Xresources &
picom &
emacs --daemon &
xfce4-power-manager &
clipmenud &
#/usr/lib/kdeconnectd &
an2linuxserver.py &
redshift &
xsetroot -cursor_name left_ptr &
flameshot &
#mpDris2 &
sxhkd -c ~/.config/sxhkd/sxhkd_dwm &
dunst &
~/.local/dwmblocks & 
xinput disable 11 &

export QT_QPA_PLATFORMTHEME="qt5ct"

while true; do
    ~/.local/dwm 2> /tmp/dwm.log
done
