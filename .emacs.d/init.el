
;;; init.el --- My Emacs Initialization/Customization file

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:


;:; ==================================================
;;; 基本設定
;;; ==================================================

;;; サブディレクトリごと load-path に追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory(expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;; load-path に追加
(add-to-load-path "site-lisp")

;;; package.el の保存先とリポジトリの追加
(setq package-user-dir "~/.emacs.d/site-lisp/elpa/")
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))
(package-initialize)

;;; use-packageと必須パッケージを自動インストール
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t)


;:; ==================================================
;;; 環境設定
;;; ==================================================

;;; 環境を日本語、UTF-8にする
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;; 曖昧な幅の文字を全角に固定する
;; See: https://github.com/hamano/locale-eaw
(use-package eaw
  :init
  (when (not (file-exists-p "~/.emacs.d/site-lisp/eaw.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/hamano/locale-eaw/master/eaw.el"
     "~/.emacs.d/site-lisp/eaw.el"))
  :config
  (eaw-fullwidth))

;;; カラーテーマの設定
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;;; メニューバーを消す
(menu-bar-mode 0)

;;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;;; バックアップファイルを作成させない
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;; window-system の場合にクリップボードを共有する
(cond (window-system
       (setq select-enable-clipboard t)))

;;; 1 行ごとにスクロールする
(setq scroll-conservatively 1)

;;; バッファを自動更新する
(global-auto-revert-mode 1)

;;; shell の環境変数引継ぎ
(use-package exec-path-from-shell
  :ensure t)


;:; ==================================================
;;; グローバルキーバインド設定
;;; ==================================================

;;; 前の文字を削除
(bind-keys :map key-translation-map ("C-h" . "<DEL>"))

;;; 前の単語を削除
(bind-keys ("M-h" . backward-kill-word))

;;; ペインの移動
;; (bind-keys ("C-c C-b" . windmove-left)
;;            ("C-c C-n" . windmove-down)
;;            ("C-c C-p" . windmove-up)
;;            ("C-c C-f" . windmove-right))

;;; バッファの手動更新
(bind-keys ([f5] . revert-buffer))

;;; Mac 用の設定
(when (equal system-type 'darwin)
  ;; Mac のキーバインドを使う
  (mac-key-mode 1)
  ;; Macのoptionをメタキーにする
  ;; 下記でうまく行かなかった場合: (setq mac-option-modifier 'meta)
  (defvar mac-option-modifier 'meta))


;:; ==================================================
;;; 独自関数定義
;;; ==================================================

;;; 選択範囲を isearch のクエリに追加する
(defadvice isearch-mode (around isearch-mode-default-string(forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;;; ウィンドウサイズを C-c w => fbpn で変更できるようにする
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action (read-key-sequence-vector (format "size[%dx%d]"
                                                       (window-width)
                                                       (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?f)
               (enlarge-window-horizontally dx))
              ((= c ?b)
               (shrink-window-horizontally dx))
              ((= c ?n)
               (enlarge-window dy))
              ((= c ?p)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
(bind-key "C-c w" 'window-resizer)


;:; ==================================================
;;; エディタ設定
;;; ==================================================

;;; タブにスペースを使用する
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;; 改行時に自動インデントする
(electric-indent-mode 1)

;;; 行番号を表示する
(use-package linum
  :config
  (global-linum-mode t)
  ;; 行番号表示箇所に3桁分の領域を確保する
  (setq linum-format "%3d "))

;;; 列数を表示する
(column-number-mode 1)

;;; カーソル行をハイライトしない
(global-hl-line-mode 0)

;;; 対応する括弧を光らせる
(use-package paren
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'mixed))

;;; 対応する括弧を補完する
(use-package elec-pair
  :config
  (electric-pair-mode t)
  (add-to-list 'electric-pair-pairs '(?{ . ?}))
  (add-to-list 'electric-pair-pairs '(?' . ?')))

;;; M-; で現在の行をコメントアウトする
(use-package comment-dwim-2
  :ensure t
  :bind (("M-;" . comment-dwim-2))
  :config
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

;;; 同じ単語をハイライトする
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :hook ((emacs-lisp-mode . highlight-symbol-mode)
         (yaml-mode . highlight-symbol-mode)
         (python-mode . highlight-symbol-mode)
         (js-mode . highlight-symbol-mode)
         (go-mode . highlight-symbol-mode))
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :config
  (setq highlight-symbol-colors
     '("DarkOrange" "DodgerBlue1" "DeepPink1"
       "goldenrod3" "orchid2" "chartreuse3"
       "yellow3" "firebrick1" "green2"
       "IndianRed3" "SeaGreen3" "cornflower blue"
       "SlateBlue2" "medium orchid" "sea green"))
  (setq highlight-symbol-idle-delay 0))

;;: インデントをハイライトする
(use-package highlight-indentation
  :ensure t
  :diminish highlight-indentation-current-column-mode
  :hook ((emacs-lisp-mode . highlight-indentation-current-column-mode)
         (yaml-mode . highlight-indentation-current-column-mode)
         (python-mode . highlight-indentation-current-column-mode)
         (js2-mode . highlight-indentation-current-column-mode)
         (go-mode . highlight-symbol-mode))
  :config
  ;; 色の指定
  (set-face-background 'highlight-indentation-face "#40483e"))

;;; ペインのレイアウトを回転させる
(use-package rotate
  :ensure t
  :bind (("M-o" . rotate-layout)
         ("C-x C-o" . rotate-window)))

;;; キーバインドで選択範囲を拡張する
(use-package expand-region
  :ensure t
  :bind (("C-\]" . er/expand-region)
         ("C-M-\]" . 'er/contract-region)))

;;; マルチカーソルを使えるようにする
(use-package multiple-cursors
  :ensure t)

;;; prefixによる連続操作のキーバインド設定
(use-package smartrep
  :ensure t
  :config
  (bind-key "C-t" nil)
  (smartrep-define-key
      global-map "C-t" '(("C-n" . 'mc/mark-next-like-this)
                         ("C-p" . 'mc/mark-previous-like-this)
                         ("C-u" . 'mc/unmark-next-like-this)
                         ("C-s" . 'mc/skip-to-next-like-this)
                         ("*"   . 'mc/mark-all-like-this))))


;:; ==================================================
;;; モード設定
;;; ==================================================

;;; dired
;; dired-x を使用する
(use-package dired-x
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("r" . wdired-change-to-wdired-mode))
  :config
  ;; dired-find-alternate-file の有効化
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 二つのdiredバッファを開いているとき、R や C のデフォルトの宛先がもう片方のディレクトリになる
  (setq dired-dwim-target t))

;; Docker コンテナ内のファイル編集
(use-package docker-tramp
  :ensure t
  :config
  (set-variable 'docker-tramp-use-names t)
  (require 'docker-tramp-compat))

;;; anzu
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode 1)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 3)
  (bind-keys ("C-c r" . anzu-query-replace)
             ("C-c R" . anzu-query-replace-regexp)))

;;; helm
;; helm本体
(use-package helm-files
  :ensure helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  (bind-keys ("M-y" . helm-show-kill-ring)
             ("M-x" . helm-M-x)
             ("C-x C-f" . helm-find-files)
             ("C-x b" . helm-for-files)
             ("C-c m" . helm-mini)
             ("C-c s" . helm-regexp)
             ("C-c g" . helm-grep-do-git-grep)
             :map helm-map
             ("C-h" . delete-backward-char)
             :map helm-find-files-map
             ("C-h" . delete-backward-char)
             ("TAB" . helm-execute-persistent-action)
             :map helm-read-file-map
             ("TAB" . helm-execute-persistent-action))
  ;; TAB で new buffer が作成されない (ファイルがない時は何もしない)
  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))
  ;; helm-buffers-list の詳細情報を非表示
  (setq helm-buffer-details-flag nil))

;; silversearcher-ag で検索を高速化
(use-package helm-ag
  :ensure t
  :config
  (setq helm-ag-insert-at-point 'symbol)
  (bind-keys ("C-c g" . helm-do-ag)))

;; helm でキーバインドを表示
(use-package helm-descbinds
  :ensure t
  :bind (("C-c b" . helm-descbinds)))

;; git プロジェクト内の全ファイル検索
(use-package helm-ls-git
  :ensure t
  :bind (("C-c l" . helm-ls-git-ls)))

;; helm で tramp を実行
(use-package helm-tramp
  :ensure t
  :commands (helm-tramp))

;; helm-swoop
(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map helm-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line)
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)
         :map helm-multi-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch))
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-horizontally)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)
  ;; If you prefer fuzzy matching
  (setq helm-swoop-use-fuzzy-match nil))

;;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

;; projectile の Helm 統合
(use-package helm-projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (bind-keys :map projectile-mode-map
             ("C-c C-p" . projectile-command-map))
  (helm-projectile-on))

;;; company (補完機能)
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)                  ; 全バッファでcompanyを有効にする
  (setq company-idle-delay 0)            ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 一番下の候補で下を押すと最初に戻る
  (setq company-dabbrev-downcase nil)    ; lowercaseで補完される機能の停止
  ;; デフォルトでは document に移動できないので company-doc-buffer を固定する
  (defun my/company-show-doc-buffer ()
    "Temporarily show the documentation buffer for the selection."
    (interactive)
    (let* ((selected (nth company-selection company-candidates))
           (doc-buffer (or (company-call-backend 'doc-buffer selected)
                           (error "No documentation available"))))
      (with-current-buffer doc-buffer
        (goto-char (point-min)))
      (display-buffer doc-buffer t)))
  
  (bind-keys ("C-M-i" . company-complete)      ; C-M-i で手動補完
             :map company-active-map
             ("M-n" . nil)                     ; 不要なキーバインドを解除
             ("M-p" . nil)                     ; 不要なキーバインドを解除
             ("C-d" . company-show-doc-buffer) ; 関数のドキュメントをミニバッファに表示
             ("C-n" . company-select-next)     ; C-n で次の補完候補を選択
             ("C-p" . company-select-previous) ; C-p で前の補完候補を選択
             ;; 候補が1つならばtabで補完、複数候補があればtabで次の候補へ行く
             ("C-i" . company-complete-common-or-cycle)
             ("M-d" . my/company-show-doc-buffer)
             :map company-search-map
             ("C-n" . company-select-next)     ; C-n で次の補完候補を選択
             ("C-p" . company-select-previous) ; C-p で前の補完候補を選択
             ))

;;; flycheck (シンタックスチェック)
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  ;; 保存時に自動チェック
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (bind-keys ("M-g l" . flycheck-list-errors)))

;;; editorconfig の有効化
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;;; yaml
(use-package yaml-mode
  :ensure t)

;;; json
(use-package json-mode
  :ensure t)

;;; docker
(use-package dockerfile-mode
  :ensure t)

;;; ansible
(use-package ansible
  :ensure t)

;;; js
(use-package js
  :mode ("\\.js\\'" . js-mode)
  :config
  (setq js-indent-level 2)
  ;; company の補完候補に jedi を追加
  (add-to-list 'company-backends 'company-tern))

(use-package company-tern
  :ensure t
  :hook ((js-mode . tern-mode)))

(use-package add-node-modules-path
  :ensure t
  :hook (js-mode . add-node-modules-path))

(use-package js-auto-format-mode
  :ensure t
  :hook (js-mode . js-auto-format-mode)
  :config
  (custom-set-variables
   '(js-auto-format-command "prettier-eslint")
   '(js-auto-format-command-args "--write")))

;;; Python
(use-package python
  :ensure company-jedi
  :ensure py-autopep8
  :ensure py-isort
  :hook ((python-mode . py-autopep8-enable-on-save))
  :config
  ;; company の補完候補に jedi を追加
  (add-to-list 'company-backends 'company-jedi)
  ;; 補完したいライブラリのパスを追加
  ;; (setenv "PYTHONPATH" "/path")
  ;; 関数定義ジャンプ
  (defvar jedi:goto-stack '())
  (defun jedi:jump-to-definition ()
    (interactive)
    (add-to-list 'jedi:goto-stack
                 (list (buffer-name) (point)))
    (jedi:goto-definition))
  (defun jedi:jump-back ()
    (interactive)
    (let ((p (pop jedi:goto-stack)))
      (if p (progn
              (switch-to-buffer (nth 0 p))
              (goto-char (nth 1 p))))))
  (bind-keys :map python-mode-map
             ("C-c C-d" . jedi:show-doc)
             ("M-." . jedi:jump-to-definition)
             ("M-," . jedi:jump-back))

  ;; autopep8 の 1 行の最大文字数を設定
  (setq py-autopep8-options '("--max-line-length=79"))
  ;; 保存時に isort を実行
  (add-hook 'before-save-hook 'py-isort-before-save))

;;; Golang
(use-package go-mode
  :ensure company-go
  :hook ((go-mode . go-eldoc-setup)
         (go-mode . company-mode)
         (go-mode . flycheck-mode)
         (go-mode . (lambda()
                      ;; company の補完候補に jedi を追加
                      (set (make-local-variable 'company-backends) '(company-go))
                      (set (make-local-variable 'compile-command)
                           "go build -v && go test -v && go vet"))))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (bind-keys :map go-mode-map
             ("M-." . godef-jump)))

;;; init.el ends here
