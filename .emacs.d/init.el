
;;; package --- init.el ---
;;; Code:


;;; 
;;; 基本設定
;;;

;;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;; ディレクトリをサブディレクトリごとload-pathに追加
(add-to-load-path "elpa" "elisp")

;;; el-getの設定
(add-to-list 'load-path (locate-user-emacs-file "el-get"))
(require 'el-get)
;; el-getでダウンロードしたパッケージは ~/.emacs.d/elisp に入るようにする
(setq el-get-dir (locate-user-emacs-file "~/.emacs.d/elisp"))

;;; 環境を日本語、UTF-8にする
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;; カラーテーマの設定
(el-get-bundle oneKelvinSmith/monokai-emacs)
(load-theme 'monokai t)

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
(set-face-background 'show-paren-match-face "grey")
(set-face-foreground 'show-paren-match-face "black")

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

;;; リモート接続(デフォルトで使える？)
;; (require 'tramp)
;; (setq tramp-default-method "scp")

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

;;; 指定行にジャンプする
(global-set-key "\C-xj" 'goto-line)

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
(el-get-bundle exec-path-from-shell)

;;; 同じ単語をハイライト
(el-get-bundle highlight-symbol)
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0)
(add-hook 'python-mode-hook 'highlight-symbol-mode)
(add-hook 'yaml-mode-hook 'highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
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
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

;;: インデントのハイライト
(el-get-bundle highlight-indentation)
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#40483e")  ; 色の指定
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'js2-jsx-mode-hook 'highlight-indentation-current-column-mode)

;;; コメントアウト
(el-get-bundle comment-dwim-2)
(require 'comment-dwim-2)
(setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
(global-set-key (kbd "M-;") 'comment-dwim-2)

;;; ペインの回転
(el-get-bundle daichirata/emacs-rotate)
(require 'rotate)
(global-set-key (kbd "M-o") 'rotate-layout)
(global-set-key (kbd "C-x C-o") 'rotate-window)

;;; 検索・置換機能の拡張
(el-get-bundle anzu)
(require 'anzu)
(global-anzu-mode +1)
(setq anzu-search-threshold 1000)
(setq anzu-minimum-input-length 3)
(global-set-key (kbd "C-c r") 'anzu-query-replace)
(global-set-key (kbd "C-c R") 'anzu-query-replace-regexp)

;;; 選択範囲を拡張
(el-get-bundle expand-region)
(require 'expand-region)
(global-set-key (kbd "C-\]") 'er/expand-region)      ; リージョンを広げる
(global-set-key (kbd "C-M-\]") 'er/contract-region)  ; リージョンを狭める

;;; 複数カーソル
(el-get-bundle multiple-cursors)
(require 'multiple-cursors)

;;; prefixによる連続操作のキーバインド設定
(el-get-bundle smartrep)
(require 'smartrep)
(global-set-key "\C-t" nil)
(smartrep-define-key
    global-map "C-t" '(("C-n" . 'mc/mark-next-like-this)
                       ("C-p" . 'mc/mark-previous-like-this)
                       ("C-u" . 'mc/unmark-next-like-this)
                       ("C-s" . 'mc/skip-to-next-like-this)
                       ("*"   . 'mc/mark-all-like-this)
                       ))

;;; helm
(el-get-bundle helm)            ; helm本体
(el-get-bundle helm-descbinds)  ; helmでキーバインドを表示
(el-get-bundle helm-ls-git)     ; gitプロジェクト内の全ファイル検索
(require 'helm-config)
(require 'helm-descbinds)
(require 'helm-ls-git)
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

;; helm-descbinds用キーバインド
(global-set-key (kbd "C-c b") 'helm-descbinds)

;; helm-ls-git用キーバインド
(global-set-key (kbd "C-c p") 'helm-ls-git-ls)
(global-set-key (kbd "M-l") 'helm-ls-git-ls)

;; TABでnew bufferが作成しない(ファイルがない時は何もしない)
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))

;; helm-buffers-list の詳細情報を非表示
(setq helm-buffer-details-flag nil)

;;; auto-completeを使用しない
(auto-complete-mode -1)

;;; company (オートコンプリート)
(el-get-bundle company-mode)
(require 'company)

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


;;; git-gutter (動作が非常に重いので普段は使わない)
(el-get-bundle git-gutter)
(require 'git-gutter)

;;; flycheck (シンタックスチェック)
(el-get-bundle flycheck)
(require 'flycheck)
(global-flycheck-mode)

;; 保存時に自動チェック
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; キーバインド
(define-key global-map (kbd "C-c n") 'flycheck-next-error)
(define-key global-map (kbd "C-c p") 'flycheck-previous-error)
(define-key global-map (kbd "C-c l") 'flycheck-list-errors)


;;; markdown
(el-get-bundle markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "gfm-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;;; yaml
(el-get-bundle yaml-mode)
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))


;;; JavaScript
(el-get-bundle js2-mode)
(el-get-bundle company-tern)
(autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)    ; jsのインデント設定
            ;; (add-hook 'js2-mode-hook 'tern-mode)
            (add-hook 'js2-jsx-mode-hook 'tern-mode)
            (add-to-list 'company-backends 'company-tern) ; backendに追加

            ;; 以下、現行バージョンのバグへの対応
            ;; https://github.com/proofit404/company-tern/issues/13
            ;; (defun company-tern-depth (candidate)
            ;;   "Return depth attribute for CANDIDATE. 'nil' entries are treated as 0."
            ;;   (let ((depth (get-text-property 0 'depth candidate)))
            ;;     (if (eq depth nil) 0 depth)))
            ))



;;; Golang
(add-hook 'go-mode-hook
          (lambda ()
            ;;; auto-complete (companyにしたい)
            (el-get-bundle auto-complete)
            (require 'auto-complete)
            (require 'auto-complete-config)
            (add-to-list 'ac-modes 'go-mode)
            (ac-set-trigger-key "TAB")
            (setq ac-use-menu-map t)      ; 補完メニュー表示時にC-n/C-pで補完候補 選択
            (setq ac-use-fuzzy t)         ; 曖昧マッチ
            (setq ac-ignore-case `smart)  ; 大文字小文字を区別しない
            ;; auto-complete の候補に日本語を含む単語が含まれないようにする
            ;; http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
            (defadvice ac-word-candidates (after remove-word-contain-japanese activate)
              (let ((contain-japanese (lambda (s) (string-match (rx (category 
                                                                     japanese)) s))))
                (setq ad-return-value
                      (remove-if contain-japanese ad-return-value))))

            ;; 初期設定
            (el-get-bundle go-mode)
            (el-get-bundle go-autocomplete)
            (el-get-bundle go-eldoc)
            (el-get-bundle go-direx)
            (add-to-list 'exec-path (expand-file-name "~/.gvm/gos/go1.5.3/bin"))
            (add-to-list 'exec-path (expand-file-name "~/.gvm/pkgsets/go1.5.3/global/bin"))
            ;; GOROOT, GOPATH環境変数の読み込み
            (let ((envs '("GOROOT" "GOPATH")))
              (exec-path-from-shell-copy-envs envs))
            (setq-default)
            ;; indentの設定
            (setq tab-width 2)
            (setq standard-indent 2)
            (setq indent-tabs-mode nil)
            ;; 自動インデント
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; godef keybind
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "M-,") 'pop-tag-mark)
            ))
(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (add-hook 'go-mode-hook 'go-eldoc-setup)))


;;; Python
(el-get-bundle jedi)
(el-get-bundle company-jedi)
(el-get-bundle py-autopep8)
(el-get-bundle yasnippet)

(add-hook 'python-mode-hook
          '(lambda()
             ;; 関数補完 (company-jedi)
             (require 'jedi-core)
             ;; (jedi:setup)
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
