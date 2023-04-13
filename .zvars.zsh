# Aquí defino mis variable (podría usar .zprofile pero nunca funcionó)

# Colorschemes OwO

#export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"

PATH="$HOME/.local/bin:/mnt/DATA/juan/color-scripts/color-scripts:$HOME/.local:$HOME/.local/share/gem/ruby/3.0.0/bin:$HOME/.config/rofi:$HOME/.emacs.d/bin:/usr/local/bin:/usr/share/kservices5/kwin:$GEM_HOME/bin:$PATH"
export PATH

export LIGHT_THEME="one-light"
export DARK_THEME="dracula"

export NVIM_LISTEN_ADDRESS=/tmp/nvimsocket #necesario para nvim --remote
export LANG=es_MX.UTF-8
export EDITOR="nvim"
#export VISUAL="nvim"
export TERMINAL="alacritty"
export PAGER=less
export OPENER="mimeopen"
[ -r "$HOME/.local/bin/lesspipe.sh" ] && export LESSOPEN="| $HOME/.local/bin/lesspipe.sh %s"
export LESS='-Ri ' #Esto es para poder ver el contenido de archivos comprimidos
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
#export XDG_CURRENT_DESKTOP=sway
export XDG_SESSION_TYPE=wayland
#export QT_QPA_PLATFORMTHEME="qt5ct" # Necesario para apps qt
export MOZ_ENABLE_WAYLAND=1
#export GTK_USE_PORTAL=0

export XKB_DEFAULT_LAYOUT=latam
#export GTK_USE_PORTAL=1
