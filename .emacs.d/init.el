

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

;;; ディレクトリをサブディレクトリごとload-pathに追加
;;; (add-to-load-path "elpa" "elisp")

;; package.elの保存先とリポジトリの追加
(setq package-user-dir "~/.emacs.d/elisp/elpa/")
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))
(package-initialize)

;; use-packageをと必須パッケージを自動インストール
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)
(require 'diminish)


;;; el-getの設定
(add-to-list 'load-path (locate-user-emacs-file "el-get"))
(require 'el-get)

;; el-getでダウンロードしたパッケージは ~/.emacs.d/elisp に入るようにする
(setq el-get-dir (locate-user-emacs-file "~/.emacs.d/elisp"))

;;; emacs の起動速度可視化
;; (require 'initchart)
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)

;;; 環境を日本語、UTF-8にする
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
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
(autoload 'dired-x "dired-x")

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

;;; 前の単語を削除
(global-set-key "\M-h" 'backward-kill-word)

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
;;; el-getを使ったパッケージ設定
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
  :diminish anzu
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
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x b") 'helm-for-files)
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
  :ensure
  :config
  ;; helm-descbinds用キーバインド
  (global-set-key (kbd "C-c b") 'helm-descbinds))


;; gitプロジェクト内の全ファイル検索
(use-package helm-ls-git
  :ensure
  :config
  (autoload 'helm-descbinds "helm-descbinds")
  (autoload 'helm-ls-git "helm-ls-git")
  ;; helm-ls-git用キーバインド
  (global-set-key (kbd "C-c l") 'helm-ls-git-ls))


;;; company (補完機能)
(use-package company
  :ensure
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
  (autoload 'flycheck "flycheck")
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
  :config
  (add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode)))


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
(use-package js2-mode
  :ensure t)

(use-package company-tern
  :ensure t
  :config
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
      )))

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)    ; jsのインデント設定
            (add-hook 'js2-jsx-mode-hook 'tern-mode)
            (add-to-list 'company-backends 'company-tern) ; backendに追加

            ;; ESlint と競合する js2-mode の機能を無効化
            (setq js2-include-browser-externs nil)
            (setq js2-mode-show-parse-errors nil)
            (setq js2-mode-show-strict-warnings nil)
            (setq js2-highlight-external-variables nil)
            (setq js2-include-jslint-globals nil)
            ))


;;; Python
;; (el-get-bundle jedi)
(use-package company-jedi
  :ensure t)
(use-package py-autopep8
  :ensure t)
(use-package yasnippet
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
             (setenv "PYTHONPATH" "/Users/sci01553/.pyenv/versions/miniconda3-4.0.5/envs/mlflow/lib/python3.5/site-packages:/Users/sci01553/.pyenv/versions/miniconda3-4.0.5/envs/data-analysys/lib/python3.5/site-packages:/home/vagrant/.pyenv/shims")
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
             (define-key python-mode-map "\C-cr" 'helm-jedi-related-names)

             ;; PEP8のチェック
             (require 'py-autopep8)
             (py-autopep8-enable-on-save)
             (add-hook 'before-save-hook 'py-autopep8-before-save)
             (setq py-autopep8-options '("--max-line-length=160"))
             (define-key python-mode-map "\C-cf" 'py-autopep8)
             
             ;; スニペット
             (require 'yasnippet)
             (yas-global-mode 1)
             ))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2da65cb7074c176ca0a33f06bcc83ef692c9175e41b6370f5e94eb5811d6ee3a" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" default)))
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode use-package smartrep rotate py-autopep8 multiple-cursors monokai-theme monokai-alt-theme markdown-mode json-mode js2-mode highlight-symbol highlight-indentation helm-ls-git helm-descbinds flycheck expand-region exec-path-from-shell dockerfile-mode company-tern company-jedi comment-dwim-2 anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
