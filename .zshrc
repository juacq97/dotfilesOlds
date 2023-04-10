# Load variables, including path. I decided to keep it on a separate file
source $HOME/.zvars.zsh
#source /etc/X11/xinit/xinitrc

# Run the graphical environment when not using a display manager.
if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
     #exec startx
     exec sway
 fi

# Powerlevel10k
 if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
 fi

# Fetch when terminal appears

if  [[ "$(uname -o)" == "Android" ]]; then
    export LANG=C
    ~/.local/bin/ufetch-termux
    source ~/.repos/powerlevel10k/powerlevel10k.zsh-theme
    source ~/.repos/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    source ~/.repos/zsh-autosuggestions/zsh-autosuggestions.zsh
else
    ufetch-noascii
    source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi



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


#source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

bindkey '^ ' autosuggest-accept #Completa con C-space
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd) #Completa con el ultimo comando que se uso despues del ultimo comando.

#source $HOME/.repos/zsh-nix-shell/nix-shell.plugin.zsh

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

# NIXOS
alias nix-install="nix-env -f '<nixpkgs>' -Ai"

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

#eval "$(starship init zsh)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export PATH=$PATH:/home/juan/.spicetify

PATH="/data/data/com.termux/files/home/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/data/data/com.termux/files/home/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/data/data/com.termux/files/home/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/data/data/com.termux/files/home/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/data/data/com.termux/files/home/perl5"; export PERL_MM_OPT;
