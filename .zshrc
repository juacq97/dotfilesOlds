# Load variables, including path. I decided to keep it on a separate file
source $HOME/.zvars.zsh

# Run the graphical environment when not using a display manager.
 if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
     exec startx
   #  sway
   #  exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) sway
 fi

# Fetch when terminal appears
#ufetch-noascii
ufetch-nixos

# Enable Powerlevel10k instant prompt.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

#Themes
# source ~/.repos/powerlevel10k/powerlevel10k.zsh-theme
# source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
# plugins=(
#     git
# )

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

bindkey -v #vim mode 
export KEYTIMEOUT=1

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
bindkey '^ ' autosuggest-accept #Completa con C-space
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd) #Completa con el ultimo comando que se uso despues del ultimo comando.

### Aliases ###
alias ls="ls --color=always"
alias grep="grep --color=always"
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
alias ref.adoc="nvim /mnt/Data/TODO/REF.adoc"
alias semana="remind -@c+1 ~/docs/DOSIFICACIONES/HORARIO.rem"

# NIXOS
alias nix-install="nix-env -f '<nixpkgs>' -Ai"

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
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Historial de zsh
HISTFILE=~/.cache/zsh/history
HISTSIZE=10000
SAVEHIST=1000
setopt SHARE_HISTORY

#lf cd on quit
LFCD="$HOME/.config/lf/lfcd.sh" 
if [ -f "$LFCD" ]; then
    source "$LFCD"
fi
alias lf="lfcd"

export BAT_THEME="base16"

f(){
    nnn-opener "$(fzf)"
}

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

eval "$(starship init zsh)"
