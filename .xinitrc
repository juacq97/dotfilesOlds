#! /bin/sh

xrandr --output DP-2 --auto --same-as LVDS-1 &
nitrogen --restore &
xrdb  ~/.Xresources &
picom &
#emacs --daemon &
xfce4-power-manager &
clipmenud &
/usr/lib/kdeconnectd &
redshift -l 24.14:110.27 -m xrandr &
xsetroot -cursor_name left_ptr &
flameshot &
#sxhkd -c ~/.config/sxhkd/sxhkd_dwm &
sxhkd -c ~/.config/sxhkd/sxhkd_exwm &
dunst &
#~/.local/dwmblocks & 
xinput disable 11 &
#fcitx5 -d &
pcloud &
#dwm-msg run_command xrdb &
xset r rate 250 60 &
export QT_QPA_PLATFORMTHEME="qt5ct"

#exec dwm
#while true; do
# ~/.local/dwm 2> /tmp/dwm.log 
#    done
#exec herbstluftwm
#exec stumpwm
exec emacs -mm --debug-init -l ~/.emacs.d/desktop.el
