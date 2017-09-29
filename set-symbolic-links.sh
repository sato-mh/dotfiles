WORK_DIR=$(echo $(cd $(dirname $0) && pwd))
ln -sf $WORK_DIR/.bashrc ~/.bashrc
cp -r $WORK_DIR/.emacs.d ~/.emacs.d
ln -sf $WORK_DIR/.emacs.d/init.el ~/.emacs.d/init.el
ln -sf $WORK_DIR/.eslintrc.js ~/.eslintrc.js
ln -sf $WORK_DIR/.flake8 ~/.flake8
ln -sf $WORK_DIR/.gitignore ~/.gitignore
ln -sf $WORK_DIR/.tern-config ~/.tern-config
ln -sf $WORK_DIR/.tmux.conf ~/.tmux.conf

exec $SHELL
