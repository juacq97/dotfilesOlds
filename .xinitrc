#! /bin/sh

nitrogen --restore &
~/.config/polybar/polybar-monitors.sh &
#wal -sR &
xrdb  ~/.Xresources &
picom &
emacs --daemon &
xfce4-power-manager &
clipmenud &
/usr/lib/kdeconnectd &
redshift &
xsetroot -cursor_name left_ptr &
flameshot &
#mpDris2 &
sxhkd -c ~/.config/sxhkd/sxhkd_dwm &
dunst &
#~/.local/dwmblocks & 
xinput disable 11 &

export QT_QPA_PLATFORMTHEME="qt5ct"

while true; do
    ~/.local/dwm 2> /tmp/dwm.log
done
