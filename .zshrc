ufetch-void
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=(
    git
)

# CASE_SENSITIVE="true"
# HYPHEN_INSENSITIVE="true"
# DISABLE_AUTO_UPDATE="true"
# export UPDATE_ZSH_DAYS=13
# DISABLE_LS_COLORS="true"
# DISABLE_AUTO_TITLE="true"
# ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
 HIST_STAMPS="mm/dd/yyyy"

source $ZSH/oh-my-zsh.sh
PATH="$HOME/scripts:/mnt/DATA/juan/color-scripts/color-scripts:$HOME/.local:$PATH"
export PATH
export LANG=es_MX.UTF-8

###### DEFINIENDO ALIAS ######
alias e="emacsclient -c"
alias et="emacsclient -t"
alias E="SUDO_EDITOR=\"emacsclient -c -a emacs\" sudoedit"
alias d="cd ~/.repos/dotfiles-lap"
alias clima="curl -s es.wttr.in/"
alias t="~/scripts/todotxt"
alias h="cd /mnt/DATA/juan"
alias r="cd ~/.repos"
alias cp="/bin/advcp -g"
alias mv="/bin/advmv -g"

export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"

export NNN_TRASH=1
export NNN_OPENER_DETACH=1
export NNN_BMS='h:/mnt/DATA/juan;r:/run/media/;d:/mnt/DATA/juan/Drive;D:~/Downloads;c:~/.config'
export LC_COLLATE="C"
export NNN_PLAIN_FILTER=1
export NNN_TMPFILE=~/.config/nnn/.lastd
export NNN_OPENER=nuke
export PAGER=less
[ -r "$HOME/scripts/lesspipe.sh" ] && export LESSOPEN="| $HOME/scripts/lesspipe.sh %s"
export LESS='-Ri '

n(){
nnn -dE "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}





# VIM-MODE activado
bindkey -v

 source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
 source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/TTY1 ]]; then
    exec startx
fi
