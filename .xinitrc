#! /bin/sh

xrandr --output DP2 --auto --same-as LVDS1 &
nitrogen --restore &
xrdb  ~/.Xresources &
picom &
#emacs --daemon &
xfce4-power-manager &
clipmenud &
/usr/lib/kdeconnectd &
redshift &
xsetroot -cursor_name left_ptr &
flameshot &
sxhkd -c ~/.config/sxhkd/sxhkd_dwm &
dunst &
~/.local/dwmblocks & 
xinput disable 11 &
#ibus-daemon -drx &
pcloud &
dwm-msg run_command xrdb &
export QT_QPA_PLATFORMTHEME="qt5ct"

#exec dwm
while true; do
     ~/.local/dwm 2> /tmp/dwm.log 
     done

