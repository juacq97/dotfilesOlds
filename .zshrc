# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="$HOME/.oh-my-zsh"

# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
# (cat ~/.cache/wal/sequences &)


##### oh my zsh ###########
ZSH_THEME="powerlevel10k/powerlevel10k"
SPACESHIP_CHAR_SYMBOL=" "
SPACESHIP_GIT_PREFIX="en "
SPACESHIP_GIT_SYMBOL=
SPACESHIP_GIT_BRANCH_PREFIX=" "
SPACESHIP_EXEC_TIME_PREFIX="Tiempo: "
SPACESHIP_VI_MODE_SHOW="true"
SPACESHIP_VI_MODE_INSERT=""
SPACESHIP_VI_MODE_NORMAL="<N>"

# Bullet-train custom
BULLETTRAIN_PROMPT_ORDER=(
    time
    context
    dir
    git
)

plugins=(
  git
#  zsh-vim-mode
)

BULLETTRAIN_CONTEXT_DEFAULT_USER=juan

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
# ZSH_CUSTOM=/path/to/new-custom-folder

source $ZSH/oh-my-zsh.sh
PATH="$HOME/scripts:$PATH"
export PATH
export LANG=es_MX.UTF-8

###### DEFINIENDO ALIAS ######

alias emc="emacsclient -c"
alias emt="emacsclient -t"
alias ranger="~/.sources/ranger/ranger.py"
alias E="SUDO_EDITOR=\"emacsclient -c -a emacs\" sudoedit"
alias dot="cd ~/.repos/.dotfiles"
alias clima="curl -s es.wttr.in/"
alias conky-reloj= "killall conky && conky -c ~/conkys/reloj-texto"
alias t="~/scripts/todotxt"
alias h="cd /mnt/DATA/juan"
##### INICIAR JUNTO CON LA TERMINAL #####
ufetch-void
#fortune-es | cowsay -f $(cat ~/cows | shuf -n1)
#fortune-es | cowsay

###### NNN #################
export NNN_TRASH=1
export NNN_OPENER_DETACH=1
export EDITOR="emacsclient -c"
#export NNN_OPS_PROG=1
export NNN_BMS='h:~;r:/media/juan;d:~/Drive;D:~/Downloads;c:~/.config'
export LC_COLLATE="C"
#export NNN_CONTEXT_COLORS="3214"
#export NNN_IDLE_TIMEOUT=900
export NNN_PLAIN_FILTER=1
#export NNN_COPIER="/home/equipo/scripts/nnn/copier"
export NNN_TMPFILE=/tmp/nnn
export NNN_OPENER=mimeopen
export PAGER=less
[ -r "$HOME/scripts/lesspipe.sh" ] && export LESSOPEN="| $HOME/scripts/lesspipe.sh %s"
export LESS='-Ri '
# Alias "n" para usar nnn con la opción cd at quit
n()
{
        nnn -dn "$@"

        if [ -f $NNN_TMPFILE ]; then
                . $NNN_TMPFILE
                rm $NNN_TMPFILE
        fi
}

# VIM-MODE activado
bindkey -v

 source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
 source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
#source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
