
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

;;; 自動インデントを無効
(electric-indent-mode 1)

;;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;;; メニューバーを消す
(menu-bar-mode -1)

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

;;; 再起動時に色々復元
(custom-set-variables '(desktop-save-mode t))
(custom-set-faces)


;;;
;;; dired設定
;;;

(require 'dired-x)
;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;; Enterを押した際に同じバッファでファイルを開く
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)


;;;
;;; キーバインド設定
;;;

;;; 指定行にジャンプする
(global-set-key "\C-xj" 'goto-line)

;;; 前の単語を削除
(global-set-key "\M-h" 'backward-kill-word)

;;; 単語選択
(global-set-key "\M-n"'mark-word)

;;; 段落選択
(global-set-key "\M-p"'mark-paragraph)

;;; ペインの移動
(global-set-key (kbd "C-c C-b")  'windmove-left)
(global-set-key (kbd "C-c C-n")  'windmove-down)
(global-set-key (kbd "C-c C-p")    'windmove-up)
(global-set-key (kbd "C-c C-f") 'windmove-right)

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
(add-hook 'yaml-mode 'highlight-symbol-mode)
(add-hook 'emacs-lisp-mode 'highlight-symbol-mode)
(add-hook 'js2-jsx-mode 'highlight-symbol-mode)

;;: インデントのハイライト
(el-get-bundle highlight-indentation)
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#40483e")  ; 色の指定
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'js2-jsx-mode 'highlight-indentation-current-column-mode)

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
(global-set-key (kbd "C-M-@") 'er/contract-region)   ; リージョンを狭める

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
                       ("C-U" . 'mc/unmark-previous-like-this)
                       ("C-s" . 'mc/skip-to-next-like-this)
                       ("C-S" . 'mc/skip-to-previous-like-this)
                       ("*"   . 'mc/mark-all-like-this)
                       ("C-@" . 'er/expand-region)))

;;; helm
(el-get-bundle helm)            ; helm本体
(el-get-bundle helm-descbinds)  ; helmでキーバインドを表示
(require 'helm-config)
(require 'helm-descbinds)
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
(global-set-key (kbd "C-c k") 'helm-descbinds)
;; TABでnew bufferが作成しない(ファイルがない時は何もしない)
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))
;; helm-buffers-list の詳細情報を非表示
(setq helm-buffer-details-flag nil)

;;; auto-complete
(el-get-bundle auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(add-to-list 'ac-modes 'js2-jsx-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ; 曖昧マッチ
(setq ac-ignore-case `smart)   ; 大文字小文字を区別しない
;; auto-complete の候補に日本語を含む単語が含まれないようにする
;; http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
(defadvice ac-word-candidates (after remove-word-contain-japanese activate)
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (setq ad-return-value
          (remove-if contain-japanese ad-return-value))))

;;; flycheck (シンタックスチェック)
(el-get-bundle flycheck)
(require 'flycheck)
(global-flycheck-mode)
;; 保存時に自動チェック
(setq flycheck-check-syntax-automatically '(mode-enabled save))
;; キーバインド
(define-key global-map (kbd "M-n") 'flycheck-next-error)
(define-key global-map (kbd "M-p") 'flycheck-previous-error)
(define-key global-map (kbd "M-l") 'flycheck-list-errors)

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
(autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
(setq js-indent-level 2)    ; jsのインデント設定
 
;;; Golang
(el-get-bundle go-mode)
(el-get-bundle go-autocomplete)
(el-get-bundle go-eldoc)
(el-get-bundle go-direx)
(add-to-list 'exec-path (expand-file-name "~/.gvm/gos/go1.5.3/bin"))
(add-to-list 'exec-path (expand-file-name "~/.gvm/pkgsets/go1.5.3/global/bin"))
(add-hook 'go-mode-hook
          (lambda ()
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
(el-get-bundle py-autopep8)
(el-get-bundle yasnippet)

(add-hook 'python-mode-hook
          '(lambda()
             ;; オートコンプリート
             (require 'jedi)
             (jedi:setup)
             ;; (jedi:ac-setup)
             (setq jedi:complete-on-dot t)
             (local-set-key (kbd "M-TAB") 'jedi:complete)
             (define-key python-mode-map "\C-cd" 'jedi:show-doc)

             ;; 補完候補をjediのもののみにする
             ;; (setq ac-sources
             ;;       (delete 'ac-source-words-in-same-mode-buffers ac-sources))
             ;; (add-to-list 'ac-sources 'ac-source-filename)
             ;; (add-to-list 'ac-sources 'ac-source-jedi-direct)  

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
             (define-key python-mode-map "\C-cj" 'jedi:jump-to-definition)
             (define-key python-mode-map "\C-cp" 'jedi:jump-back)
             (define-key python-mode-map "\C-cr" 'helm-jedi-related-names)

             ;; PEP8のチェック
             (require 'py-autopep8)
             (py-autopep8-enable-on-save)
             (add-hook 'before-save-hook 'py-autopep8-before-save)
             (setq py-autopep8-options '("--max-line-length=160"))
             (define-key python-mode-map (kbd "C-c f") 'py-autopep8)
             
             ;; スニペット
             (require 'yasnippet)
             (yas-global-mode 1)
             ))

;;; init.el ends here
