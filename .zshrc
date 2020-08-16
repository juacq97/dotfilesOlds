### Definiendo path ###
PATH="$HOME/.local/bin:/mnt/DATA/juan/color-scripts/color-scripts:$HOME/.local:$PATH:$HOME/.gem/ruby/2.7.0/bin:$HOME/.config/rofi"
export PATH

### Variables de entorno ###
export QT_QPA_PLATFORMTHEME="qt5ct"
export WM=dwm
export LANG=es_MX.UTF-8
export EDITOR="vim"
export VISUAL="erwap"
#export PLANS="/mnt/DATA/juan/Drive/SEC-ABREOJOS/PLANS"
export PAGER=less
export ZSH="$HOME/.oh-my-zsh"
export OPENER="mimeopen"
[ -r "$HOME/.local/bin/lesspipe.sh" ] && export LESSOPEN="| $HOME/.local/bin/lesspipe.sh %s"
export LESS='-Ri ' #Esto es para poder ver el contenido de archivos comprimidos

### Ejecuta xinit si es la TTY 1 ###
if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
fi

### Ejecuta al abrir terminales (casi siempre ufetch) ###
ufetch-arch

### Instant prompt para p10k ###
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

### Oh My Zsh config ###
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=(
    git
)

DISABLE_AUTO_TITLE="false"
#ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"

#bindkey -v #vim mode 
source $ZSH/oh-my-zsh.sh

# Syntax highlight!
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Vim mode
source /usr/share/zsh/plugins/zsh-vim-mode/zsh-vim-mode.plugin.zsh

# Autocompletado
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
bindkey '^ ' autosuggest-accept #Completa con C-space
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd) #Completa con el ultimo comando que se uso despues del ultimo comando.


### Aliases ###
alias e="emacsclient -c"
alias emc="emacsclient -c"
alias et="emacsclient -t"
alias E="SUDO_EDITOR=\"emacsclient -c -a emacs\" sudoedit"
alias d="cd ~/.repos/dotfiles"
alias clima="curl -s es.wttr.in/"
alias h="cd /mnt/Data"
alias r="cd ~/.repos"
alias lf="lfwrap"
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
