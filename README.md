# はじめに
個人的な設定ファイルです。

# セットアップ
1. 設定ファイル

   ```shell
   git clone https://github.com/sato-mh/dotfiles.git
   cp -r dotfiles/ubuntu14.0.4/.* .
   ```

1. emacs
   - インストール
   
    ```shell
    mkdir ~/dlp
    cd ~/dlp
    wget http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-24.5.tar.gz
    tar -zxf emacs-24.5.tar.gz
    cd emacs-24.5/
    sudo apt-get install gcc make ncurses-dev
    ./configure && make
    sudo make install
    ```

    - elpy

    ```shell
    emacs ~/.emacs.d/init.el  # 全て"y"で起動
    # 282行目の(el-get-bundle elpy)をコメントアウトする
    cp ~/.emacs.d/my-modules/idomenu.el ~/.emacs.d/elisp/idomenu/
    cp ~/.emacs.d/my-modules/elpy.elc ~/.emacs.d/elisp/elpy/
    ```

1. tmux

   ```shell
   sudo apt-get build-dep tmux
   sudo apt-get install ncurses-dev
   cd ~/dlp
   git clone https://github.com/tmux/tmux.git
   cd /usr/local/src/tmux
   sh autogen.sh
   ./configure && make
   sudo make install
   ```

1. python環境構築
   - pyenvのインストール

   ```shell
   curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
   source ~/.bashrc
   ```

   - minicondaのインストール

   ```shell
   pyenv install miniconda3-3.19.0
   pyenv global miniconda3-3.19.0
   ```

   - 必要なライブラリのインストール

   ```shell
   pip install rope jedi flake8 importmagic autopep8 yapf
   ```

1. node環境構築
   - nvmのインストール

   ```shell
   git clone git://github.com/creationix/nvm.git ~/.nvm
   source ~/.bashrc
   ```

   - nodeのインストール

   ```shell
   nvm install [version]
   npm update -g npm
   ```

1. タイムゾーンの設定

```shell
timedatectl set-timezone Asia/Tokyo
```