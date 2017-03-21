#!/bin/bash
# This is the script to setup my environment
# Usage: bash setup.sh

# Specified variables
readonly HOST_NAME=ubuntu16
readonly EMACS_VER=emacs-25.1
readonly MINICONDA_VER=miniconda3-4.1.11
readonly NODE_VER=v6.10.0


#
# Setup fundamental Environment
#

# Create symbolic link
WORK_DIR=$(echo $(cd $(dirname $0) && pwd))
ln -sf $WORK_DIR/.bashrc ~/.bashrc
ln -sf $WORK_DIR/.flake8 ~/.flake8
ln -sf $WORK_DIR/.tmux.conf ~/.tmux.conf
cp -r $WORK_DIR/.emacs.d ~/.emacs.d
ln -sf $WORK_DIR/.emacs.d/init.el ~/.emacs.d/init.el

# Set host name
sudo -E sh -c "echo $HOST_NAME > /etc/hostname"

# Set timezone
sudo -E timedatectl set-timezone Asia/Tokyo

# Setting for git
git config --global credential.helper cache
git config --global core.editor emacs

# update apt package list
sudo -E apt update

# Install emacs
mkdir ~/dlp
cd ~/dlp
wget http://ftp.jaist.ac.jp/pub/GNU/emacs/$EMACS_VER.tar.gz
tar -zxf $EMACS_VER.tar.gz
cd $EMACS_VER
./configure && make
sudo -E make install

# Git clone el-get
cd ~/.emacs.d
git clone https://github.com/dimitri/el-get.git
    
# Install tmux
sudo -E apt install -y ncurses-dev libevent-dev automake
cd ~/dlp
git clone https://github.com/tmux/tmux.git
cd tmux
sh autogen.sh
./configure && make
sudo -E make install

# Install others
sudo -E apt -y tig


#
# Setup Python Environment
#

# Install pyenv
curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
source ~/.bashrc

# Install miniconda
pyenv install $MINICONDA_VER
pyenv activate $MINICONDA_VER
pyenv virtualenv global
pyenv global global
pyenv activate global

# Install library required my python environment
pip install flake8 autopep8

# Install library required by jedi
# pip install virtualenv
# pip install --upgrade ~/.emacs.d/elisp/jedi/


#
# Setup Node Environment
#

# Install nvm
git clone git://github.com/creationix/nvm.git ~/.nvm
source ~/.bashrc

# Install node
nvm install $NODE_VER
npm update -g npm

