# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Alias
case "${OSTYPE}" in
    darwin*)
        export LSCOLORS=gxfxcxdxbxegedabagacad
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
alias e="emacsclient -nw"
alias grep="grep --color"
alias gopath="cd $GOPATH"
alias sudo="sudo -E"
alias relogin="exec -l $SHELL"
alias git-rm-merged-branch="git branch --merged master | grep -vE '^\*|master$|develop$' | xargs -I % git branch -d %"
alias git-rm-remote-merged-branach="git branch -r --merged master | grep -v -e master -e develop | sed -e 's% *origin/%%' | xargs -I% git push --delete origin %"
alias docker-rm-all-container="docker ps -a -q | xargs docker rm -f"
alias docker-run-with-my-env="docker run -it -v ~/dotfiles:/root/dotfiles -v ~/projects/:/root/projects -v ~/.pyenv:/root/.pyenv"

# undef default keybind
stty stop undef

# history
function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
shopt -u histappend
export HISTSIZE=10000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M:%S '

# terminal colors
TERM=xterm-256color

# PS1
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

# pyenv
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# nvm
if [[ -s ~/.nvm/nvm.sh ]];
 then source ~/.nvm/nvm.sh
fi

# GVM
# export PATH=$PATH:$HOME/.gvm/bin
# [[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
# gvm use go1.5.3    

# mysql by docker
export MYSQL_HOST=0.0.0.0
