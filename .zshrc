
### Definiendo path ###
PATH="$HOME/.local/bin:/mnt/DATA/juan/color-scripts/color-scripts:$HOME/.local:$PATH:$HOME/.gem/ruby/2.7.0/bin:$HOME/.config/rofi:$HOME/.emacs.d/bin"
export PATH

### Variables de entorno ###
#export QT_QPA_PLATFORMTHEME="qt5ct"
#export WM=dwm
export LANG=es_MX.UTF-8
export EDITOR="vim"
export VISUAL="erwap"
#export PLANS="/mnt/DATA/juan/Drive/SEC-ABREOJOS/PLANS"
### nihongo
#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=ibus
#export QT_IM_MODULE=ibus
export PAGER=less
#export ZSH="$HOME/.oh-my-zsh"
#export OPENER="mimeopen"
[ -r "$HOME/.local/bin/lesspipe.sh" ] && export LESSOPEN="| $HOME/.local/bin/lesspipe.sh %s"
export LESS='-Ri ' #Esto es para poder ver el contenido de archivos comprimidos
# LF_ICONS
#. ~/.config/lf/LF_ICONS


### Ejecuta xinit si es la TTY 1 ###
#if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
#    exec startx
#fi

### Ejecuta al abrir terminales (casi siempre ufetch) ###
ufetch-fedora

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#Themes
source ~/.repos/powerlevel10k/powerlevel10k.zsh-theme
plugins=(
    git
)


bindkey -v #vim mode 

# Syntax highlight!
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Vim mode
#source /usr/share/zsh-vim-mode/zsh-vim-mode.plugin.zsh

# Autocompletado
source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
bindkey '^ ' autosuggest-accept #Completa con C-space
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd) #Completa con el ultimo comando que se uso despues del ultimo comando.

# Autocomplete
#source ~/.repos/zsh-autocomplete/zsh-autocomplete.plugin.zsh
#zstyle ':autocomplete:tab:*' widget-style menu-select

### Aliases ###
alias e="emacsclient -c"
alias emc="emacsclient -c"
alias et="emacsclient -t"
alias E="SUDO_EDITOR=\"emacsclient -c -a emacs\" sudoedit"
alias d="cd ~/.repos/dotfiles"
alias clima="curl -s es.wttr.in/"
alias h="cd /mnt/Data"
alias r="cd ~/.repos"
#alias lf="lfwrap"
alias vim="nvim"
alias nnn="nwrap"
alias v="nvim"
#alias cp="/bin/advcp -g"
#alias mv="/bin/advmv -g"

### cd on quit para nnn ###
NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
n(){
nwrap "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

# Usa p10k como tema para Oh my Zsh
#[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
#source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


#Update picom
update-picom()
if [[ $(pgrep picom) -ge "0" ]]; then
    pkill picom & picom & disown
else
   picom & disown
fi

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=1000
setopt SHARE_HISTORY
