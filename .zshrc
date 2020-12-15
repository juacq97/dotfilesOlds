### Definiendo path ###
PATH="$HOME/.local/bin:/mnt/DATA/juan/color-scripts/color-scripts:$HOME/.local:$PATH:$HOME/.gem/ruby/2.7.0/bin:$HOME/.config/rofi:$HOME/.emacs.d/bin"
export PATH
source $HOME/.zvars

### Ejecuta xinit si es la TTY 1 ###
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx
fi

### Ejecuta al abrir terminales 
ufetch-noascii

# Enable Powerlevel10k instant prompt.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#Themes
source ~/.repos/powerlevel10k/powerlevel10k.zsh-theme
plugins=(
    git
)

source $ZSH/oh-my-zsh.sh
bindkey -v #vim mode 
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
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
alias ncmd="nnn -dcE"
alias todo="todotxt"
alias n="ncmd"

### cd on quit para nnn ###
#NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
#nnn(){
#nnn "$@"
#    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
#        echo "nnn is already running"
#        return
#    fi
#
#    if [ -f "$NNN_TMPFILE" ]; then
#            . "$NNN_TMPFILE"
#            rm -f "$NNN_TMPFILE" > /dev/null
#    fi
#}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Historial de zsh
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=1000
setopt SHARE_HISTORY

#lf cd on quit
LFCD="$HOME/.config/lf/lfcd.sh" 
if [ -f "$LFCD" ]; then
    source "$LFCD"
fi
alias lf="lfcd"

export BAT_THEME="gruvbox"

f(){
    nnn-opener "$(fzf)"
}


alias hackear-a-isabella="hollywood"
