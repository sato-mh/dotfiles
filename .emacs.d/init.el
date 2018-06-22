

;;; package --- init.el ---
;;; Code:

;;; 
;;; 基本設定
;;;

;;; load-pathを追加する関数を定義

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; ディレクトリをサブディレクトリごとload-pathに追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;; package.elの保存先とリポジトリの追加
(setq package-user-dir "~/.emacs.d/elisp/elpa/")
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))
(package-initialize)

;;; use-packageをと必須パッケージを自動インストール
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)
(use-package diminish
  :ensure t)

;;; 環境を日本語、UTF-8にする
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;; カラーテーマの設定
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;;; バックアップファイルを作成させない
(setq make-backup-files nil)
(setq auto-save-default nil)

;;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;; タブにスペースを使用する
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;; インデント変更
(setq-default c-basic-offset 2)

;;; 自動インデントを有効
(electric-indent-mode 1)

;;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;;; メニューバーを消す
(menu-bar-mode -1)

;;; 行番号を表示する
(global-linum-mode t)
;; 行番号表示箇所に3桁分の領域を確保
(setq linum-format "%3d ")

;;; 列数を表示する
(column-number-mode t)

;;; 行数を表示する
(global-linum-mode t)

;;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;;; カーソル行をハイライトする
(global-hl-line-mode t)

;;; 対応する括弧を光らせる
(show-paren-mode 1)

;;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(setq show-paren-style 'mixed)
;; (set-face-background 'show-paren-match-face "grey")
;; (set-face-foreground 'show-paren-match-face "black")

;;; スクロールは１行ごとに
(setq scroll-conservatively 1)

;;; 対応する括弧を補完
(electric-pair-mode t)
(add-to-list 'electric-pair-pairs '(?{ . ?}))
(add-to-list 'electric-pair-pairs '(?' . ?'))

;;; クリップボードの共有
(setq x-select-enable-clipboard t)

;;; バッファの自動更新
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;;; 選択範囲をisearch
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
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

;;; ウィンドウサイズの変更
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
(global-set-key "\C-cw" 'window-resizer)


;;;
;;; dired設定
;;;

;;; dired-x を使用
(require 'dired-x)

;;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;;; Enterを押した際に同じバッファでファイルを開く
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;;;  二つのdiredバッファを開いているとき、RやCのデフォルトの宛先がもう片方のディレクトリになる
(setq dired-dwim-target t)

;;; rを押した際にファイル名をまとめて編集可能なWDiredモードに入る
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)


;;;
;;; キーバインド設定
;;;

;;; 前の文字を削除
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;;; 前の単語を削除
(global-set-key (kbd "M-h") 'backward-kill-word)

;;; ペインの移動
(global-set-key (kbd "\C-c C-b")  'windmove-left)
(global-set-key (kbd "\C-c C-n")  'windmove-down)
(global-set-key (kbd "\C-c C-p")    'windmove-up)
(global-set-key (kbd "\C-c C-f") 'windmove-right)

;;; バッファの手動更新
(global-set-key [f5] 'revert-buffer)

;;; Macのキーバインドを使う
;; (mac-key-mode 1)

;;; Macのoptionをメタキーにする
(setq mac-option-modifier 'meta)


;;;
;;; パッケージ設定
;;;

;;; shellの環境変数引継ぎ
(use-package exec-path-from-shell
  :ensure t)

;;; 同じ単語をハイライト
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0)
  (add-hook 'python-mode-hook 'highlight-symbol-mode)
  (add-hook 'yaml-mode-hook 'highlight-symbol-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'js2-mode-hook 'highlight-symbol-mode)
  (add-hook 'js2-jsx-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-colors
     '(
       "DarkOrange" "DodgerBlue1" "DeepPink1"
       "goldenrod3" "orchid2" "chartreuse3"
       "yellow3" "firebrick1" "green2"
       "IndianRed3" "SeaGreen3" "cornflower blue"
       "SlateBlue2" "medium orchid" "sea green"
       ))
  (global-set-key (kbd "M-n") 'highlight-symbol-next)
  (global-set-key (kbd "M-p") 'highlight-symbol-prev))

;;: インデントのハイライト
(use-package highlight-indentation
  :ensure t
  :config
  ;; (set-face-background 'highlight-indentation-face "#40483e")  ; 色の指定
  (add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-indentation-current-column-mode)
  (add-hook 'js2-mode-hook 'highlight-indentation-current-column-mode)
  (add-hook 'js2-jsx-mode-hook 'highlight-indentation-current-column-mode))

;;; コメントアウト
(use-package comment-dwim-2
  :ensure t
  :config
  (autoload 'comment-dwim-2 "comment-dwim-2")
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
  (global-set-key (kbd "M-;") 'comment-dwim-2))

;;; ペインの回転
(use-package rotate
  :ensure t
  :config
  (autoload 'rotate "rotate")
  (global-set-key (kbd "M-o") 'rotate-layout)
  (global-set-key (kbd "C-x C-o") 'rotate-window))

;;; 検索・置換機能の拡張
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (autoload 'anzu "anzu")
  (global-anzu-mode +1)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 3)
  (global-set-key (kbd "C-c r") 'anzu-query-replace)
  (global-set-key (kbd "C-c R") 'anzu-query-replace-regexp))

;;; 選択範囲を拡張
(use-package expand-region
  :ensure t
  :config
  (autoload 'expand-region "expand-region")
  (global-set-key (kbd "C-\]") 'er/expand-region)       ; リージョンを広げる
  (global-set-key (kbd "C-M-\]") 'er/contract-region))  ; リージョンを狭める

;;; 複数カーソル
(use-package multiple-cursors
  :ensure t
  :config
  (autoload 'multiple-cursors "multiple-cursors"))

;;; prefixによる連続操作のキーバインド設定
(use-package smartrep
  :ensure t
  :config
  (global-set-key "\C-t" nil)
  (smartrep-define-key
      global-map "C-t" '(("C-n" . 'mc/mark-next-like-this)
                         ("C-p" . 'mc/mark-previous-like-this)
                         ("C-u" . 'mc/unmark-next-like-this)
                         ("C-s" . 'mc/skip-to-next-like-this)
                         ("*"   . 'mc/mark-all-like-this))))

;;; helm
;; helm本体
(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  ;; helm用キーバインド
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x b") 'helm-for-files)
  (define-key global-map (kbd "C-c m") 'helm-mini)
  (define-key global-map (kbd "C-c s") 'helm-regexp)
  (define-key global-map (kbd "C-c g") 'helm-grep-do-git-grep)

  ;; TABでnew bufferが作成しない(ファイルがない時は何もしない)
  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  ;; helm-buffers-list の詳細情報を非表示
  (setq helm-buffer-details-flag nil))


;; helmでキーバインドを表示
(use-package helm-descbinds
  :ensure t
  :config
  ;; helm-descbinds用キーバインド

  (global-set-key (kbd "C-c b") 'helm-descbinds))


;; gitプロジェクト内の全ファイル検索
(use-package helm-ls-git
  :ensure t
  :config
  (global-set-key (kbd "C-c l") 'helm-ls-git-ls))


;; helmでtrampを実行
(use-package helm-tramp
  :ensure t)

;; helm-swoop
(use-package helm-swoop
  :ensure t
  :config
  ;; Change the keybinds to whatever you like :)
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

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

;; プロジェクト管理機能
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

;; プロジェクト管理機能の Helm 統合
(use-package helm-projectile
  :ensure t
  :config
  ;; (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;;; company (補完機能)
(use-package company
  :ensure t
  :config
  (autoload 'company "company")
  ;; 基本設定
  (global-company-mode)                   ; 全バッファでcompanyを有効にする
  (setq company-idle-delay 0)             ; デフォルトは0.5
  (setq company-minimum-prefix-length 2)  ; デフォルトは4
  (setq company-selection-wrap-around t)  ; 一番下の候補で下を押すと最初に戻る
  (setq company-dabbrev-downcase nil)     ; lowercaseで補完される機能の停止

  ;; 不要なキーバインドを解除
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)

  ;; C-M-i で手動補完
  (global-set-key (kbd "C-M-i") 'company-complete)

  ;; 関数のドキュメントをミニバッファに表示
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)

  ;; 候補が1つならばtabで補完、複数候補があればtabで次の候補へ行く
  (define-key company-active-map (kbd "C-i") 'company-complete-common-or-cycle)

  ;; デフォルトではdocumentに移動できないのでcompany-doc-bufferを固定する
  (defun my/company-show-doc-buffer ()
    "Temporarily show the documentation buffer for the selection."
    (interactive)
    (let* ((selected (nth company-selection company-candidates))
           (doc-buffer (or (company-call-backend 'doc-buffer selected)
                           (error "No documentation available"))))
      (with-current-buffer doc-buffer
        (goto-char (point-min)))
      (display-buffer doc-buffer t)))
  (define-key company-active-map (kbd "M-d") #'my/company-show-doc-buffer)

  ;; 関数のドキュメントをポップアップ表示(terminal上では動作しない)
  ;; (el-get-bundle company-quickhelp)
  ;; (el-get-bundle pos-tip)
  ;; (require 'company-quickhelp)
  ;; (company-quickhelp-mode 1)
  )


;;; flycheck (シンタックスチェック)
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)


  ;; 保存時に自動チェック
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  ;; キーバインド
  ;; (define-key global-map (kbd "C-c n") 'flycheck-next-error)
  ;; (define-key global-map (kbd "C-c p") 'flycheck-previous-error)
  ;; (define-key global-map (kbd "C-c l") 'flycheck-list-errors)
  (define-key global-map (kbd "M-g l") 'flycheck-list-errors))


;;; markdown
(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (autoload 'gfm-mode "gfm-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))


;;; yaml
(use-package yaml-mode
  :ensure t
  :mode (("\\.raml\\'" . yaml-mode)))


;;; json
(use-package json-mode
   :ensure t
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  )


;;; docker
(use-package dockerfile-mode
  :ensure t)

;;; JavaScript
(use-package company-tern
  :ensure t
  :mode (("\\.js$" . js2-mode))
  ;; :mode (("\\.js$" . js2-jsx-mode))
  )

(use-package js2-mode
  :ensure t
  :config
  (setq js-indent-level 2)    ; jsのインデント設定
  (add-hook 'js2-mode-hook 'tern-mode)
  ;; (add-hook 'js2-jsx-mode-hook 'tern-mode)
  (add-to-list 'company-backends 'company-tern)  ; backendに追加
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
      ))
  
  ;; ESlint と競合する js2-mode の機能を無効化
  (setq js2-include-browser-externs nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-highlight-external-variables nil)
  (setq js2-include-jslint-globals nil)
  )

(use-package js-auto-format-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'js-auto-format-mode)
  (custom-set-variables
   '(js-auto-format-command "prettier")
   '(js-auto-format-command-args "--write --single-quote --no-semi"))
  )

;;; Python
(use-package company-jedi
  :ensure t)
(use-package py-autopep8
  :ensure t)
(use-package yasnippet
  :ensure t)
(use-package py-isort
  :ensure t)

(add-hook 'python-mode-hook
          '(lambda()
             ;; 関数補完 (company-jedi)
             (require 'jedi-core)
             (setq jedi:complete-on-dot t)
             (setq jedi:use-shortcuts t)
             (add-to-list 'company-backends 'company-jedi) ; backendに追加
             ;; 補完したいライブラリのパスを追加
             ;; (setenv "PYTHONPATH" "/path")
             ;; (setenv "PYTHONPATH" "/home/vagrant/.pyenv/versions/miniconda3-4.0.5/envs/mlflow/lib/python3.5/site-packages:/home/vagrant/.pyenv/versions/miniconda3-4.0.5/envs/data-analysys/lib/python3.5/site-packages:/home/vagrant/.pyenv/shims")
             ;; (setenv "PYTHONPATH" "/Users/sci01553/.pyenv/versions/miniconda3-4.0.5/envs/mlflow/lib/python3.5/site-packages:/Users/sci01553/.pyenv/versions/miniconda3-4.0.5/envs/data-analysys/lib/python3.5/site-packages:/home/vagrant/.pyenv/shims")
             (define-key python-mode-map "\C-c \C-d" 'jedi:show-doc)

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
             (define-key python-mode-map "\M-." 'jedi:jump-to-definition)
             (define-key python-mode-map "\M-," 'jedi:jump-back)
             ;; (define-key python-mode-map "\C-c r" 'helm-jedi-related-names)

             ;; PEP8のチェック
             (require 'py-autopep8)
             (py-autopep8-enable-on-save)
             (add-hook 'before-save-hook 'py-autopep8-before-save)
             (setq py-autopep8-options '("--max-line-length=160"))
             (define-key python-mode-map "\C-c f" 'py-autopep8)
             
             ;; スニペット
             (require 'yasnippet)
             (yas-global-mode 1)

             ;; isort を保存前に実行
             (require 'py-isort)
             (add-hook 'before-save-hook 'py-isort-before-save)
             ))

;;; init.el ends here
