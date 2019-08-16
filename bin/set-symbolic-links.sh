#!/bin/bash

WORK_DIR=$(echo $(cd $(dirname $0)/../ && pwd))
ln -sf $WORK_DIR/linux/.bashrc ~/.bashrc
mkdir -p ~/.emacs.d/site-lisp
ln -sf $WORK_DIR/.emacs.d/init.el ~/.emacs.d/init.el
ln -sf $WORK_DIR/linux/.eslintrc.js ~/.eslintrc.js
ln -sf $WORK_DIR/linux/.flake8 ~/.flake8
mkdir -p ~/.config/yapf
ln -sf $WORK_DIR/linux/config/yapf/.style.yapf ~/.config/yapf/style
ln -sf $WORK_DIR/linux/.gitignore ~/.gitignore
ln -sf $WORK_DIR/linux/.inputrc ~/.inputrc
ln -sf $WORK_DIR/linux/.tern-config ~/.tern-config
ln -sf $WORK_DIR/linux/.tmux.conf ~/.tmux.conf

exec $SHELL
