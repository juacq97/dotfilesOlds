# Aquí defino mis variable (podría usar .zprofile pero nunca funcionó)

# Colorschemes OwO

PATH="$HOME/.local/bin:/mnt/DATA/juan/color-scripts/color-scripts:$HOME/.local:$PATH:$HOME/.gem/ruby/2.7.0/bin:$HOME/.config/rofi:$HOME/.emacs.d/bin:/usr/local/bin:/usr/share/kservices5/kwin"
export PATH

export LIGHT_THEME="one-light"
export DARK_THEME="dracula"

export NVIM_LISTEN_ADDRESS=/tmp/nvimsocket #necesario para nvim --remote
#export QT_QPA_PLATFORMTHEME="qt5ct" # Necesario para apps qt
#export WM=herbstluftwm # Necesario para ufetch
export LANG=es_MX.UTF-8
#export EDITOR="nvim"
#export VISUAL="nvim"
export TERMINAL="alacritty"
export PAGER=less
#export ZSH="$HOME/.oh-my-zsh"
export OPENER="mimeopen"
[ -r "$HOME/.local/bin/lesspipe.sh" ] && export LESSOPEN="| $HOME/.local/bin/lesspipe.sh %s"
export LESS='-Ri ' #Esto es para poder ver el contenido de archivos comprimidos
#export $(dbus-launch) #Variable para dbus
export WINIT_X11_SCALE_FACTOR=1 #No estoy sguro, pero ayuda a alacritty en multiples monitores
. ~/.config/lf/LF_ICONS #íconos para lf
. ~/.local/bin/nwrap
# FZF variables
export FZF_DEFAULT_COMMAND="find . "
#
#The next section is needed by ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
GLFW_IM_MODULE=ibus
INPUT_METHOD=ibus
IMSETTINGS_MODULE=ibus

export NIXOS_CONFIG="$HOME/.repos/dotfiles/nixos/configuration.nix"
# This is for zoom on wayland
#XDG_CURRENT_DESKTOP=gnome
