# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Alias
case "${OSTYPE}" in
    darwin*)
        alias l="ls -lhG"
	      alias l.="ls -lhG -d .*"
	      alias ll="ls -lahG"
	      alias ls="ls -G"
	      ;;
    linux*)
        alias l="ls -lh --color=auto"
	      alias l.="ls -lh -d .* --color=auto"
	      alias ll="ls -lah --color=auto"
	      alias ls="ls --color=auto"
	      ;;
esac
alias grep="grep --color"
alias gopath="cd $GOPATH"
alias sudo="sudo -E"
alias relogin="exec -l $SHELL"
alias remove-merged-branch="git branch --merged master | grep -vE '^\*|master$|develop$' | xargs -I % git branch -d %"
alias remove-remote-merged-branach="git branch -r --merged master | grep -v -e master -e develop | sed -e 's% *origin/%%' | xargs -I% git push --delete origin %"
# User specific aliases and functions
stty stop undef

# Setting for history
function share_history {
    history -a
    history -c
    history -r
    
}
PROMPT_COMMAND='share_history'
shopt -u histappend
export HISTSIZE=10000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M:%S '

# Setting for terminal colors
TERM=xterm-256color

# Setting for PS1
# PS1="[\u@\h \W]\\$ "    # デフォルト設定
function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}
function promps {
    # 色の変数宣言
    local BLACK="\[\e[0;30m\]"
    local GRAY="\[\e[1;30m\]"
    local LIGHT_GRAY="\[\e[0;37m\]"
    local WHITE="\[\e[1;37m\]"
    local RED="\[\e[0;31m\]"
    local LIGHT_RED="\[\e[1;31m\]"
    local GREEN="\[\e[0;32m\]"
    local LIGHT_GREEN="\[\e[1;32m\]"
    local BROWN="\[\e[0;33m\]"
    local YELLOW="\[\e[1;33m\]"
    local BLUE="\[\e[0;34m\]"
    local LIGHT_BLUE="\[\e[1;34m\]"
    local PURPLE="\[\e[0;35m\]"
    local MAGENTA="\[\e[1;35m\]"
    local CYAN="\[\e[0;36m\]"
    local LIGHT_CYAN="\[\e[1;36m\]"

    case $TERM in
        xterm*) TITLEBAR='\[\e]0;\W\007\]';;
        *)      TITLEBAR="";;
    esac
    local BASE="\u@\h"
    PS1="${TITLEBAR}${LIGHT_RED}${BASE}${WHITE}:${YELLOW}\W${LIGHT_GREEN}\$(parse_git_branch)${YELLOW}\$${WHITE} "
}
promps

# Setting for bash_completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Setting for pyenv
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# Setting for nvm
if [[ -s ~/.nvm/nvm.sh ]];
 then source ~/.nvm/nvm.sh
fi

# Setting for GVM
# export PATH=$PATH:$HOME/.gvm/bin
# [[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
# gvm use go1.5.3    

