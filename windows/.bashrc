# ===================
# Basic
# ===================

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export TERM=xterm-256color
export EDITOR=emacs

# undef default keybind
stty stop undef


# ===================
# History
# ===================

function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
shopt -u histappend
export HISTSIZE=50000
export HISTTIMEFORMAT='%Y/%m/%d %H:%M:%S '


# ===================
# Alias
# ===================

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
    msys*)
        alias l="ls -lh --color=auto"
	      alias l.="ls -lh -d .* --color=auto"
	      alias ll="ls -lah --color=auto"
	      alias ls="ls --color=auto"
	      ;;
esac
alias grep="grep --color"
alias sudo="sudo -E"
alias relogin="exec -l $SHELL"
alias gopath="cd $GOPATH"

# for git
alias git-rm-merged-branch="git branch --merged master | grep -vE '^\*|master$|develop$' | xargs -I % git branch -d %"
alias git-rm-remote-merged-branch="git branch -r --merged master | grep -v -e master -e develop | sed -e 's% *origin/%%' | xargs -I% git push --delete origin %"

# for docker
alias docker-rm-all-container="docker ps -a -q | xargs docker rm -f"
alias docker-run-with-my-env="docker run -it -v ~/dotfiles:/root/dotfiles -v ~/projects/:/root/projects -v ~/.pyenv:/root/.pyenv"

# for emacs
function estart() {
  if ! emacsclient -e 0 > /dev/null 2>&1; then
          cd > /dev/null 2>&1
    emacs --daemon
    cd - > /dev/null 2>&1
  fi
}
alias e='estart && emacsclient -nw'
alias ekill="emacsclient -e '(kill-emacs)'"


# ===================
# PS1
# デフォルト設定 (PS1="[\u@\h \W]\\$ ")
# ===================

# function parse_git_branch {
#     git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
# }

source /usr/share/git/git-prompt.sh
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
    # PS1="${TITLEBAR}${LIGHT_RED}${BASE}${WHITE}:${YELLOW}\W${LIGHT_GREEN}\$(parse_git_branch)\n${YELLOW}\$${WHITE} "
    PS1="${TITLEBAR}${LIGHT_RED}${BASE}${WHITE}:${YELLOW}\W${LIGHT_GREEN}\`__git_ps1\`\n${YELLOW}\$${WHITE} "
}
promps


# ===================
# Others
# ===================

# bash-completion
if [ -f /usr/local/share/bash-completion/bash_completion ]; then
    . /usr/local/share/bash-completion/bash_completion
elif [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# anyenv
if [ -d $HOME/.anyenv ]
then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -)"
fi

# pipenv
_pipenv_completion() {
    local IFS=$'\t'
    COMPREPLY=( $( env COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   _PIPENV_COMPLETE=complete-bash $1 ) )
    return 0
}
complete -F _pipenv_completion -o default pipenv

# mysql by docker
export MYSQL_HOST=0.0.0.0

# run ssh-agent
if [ ! -f ~/.ssh-agent.out ]; then
    . .dotfiles/bin/start-ssh-agent.sh
fi
eval $(cat ~/.ssh-agent.out) 1> /dev/null

# gopath
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# mingw
export PATH=/mingw64/bin/:$PATH
