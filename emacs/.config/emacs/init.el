;;; init.el --- Personal Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Startup-critical settings live in early-init.el.  This file configures
;; packages with `use-package', which ships with Emacs 29 and later.
;;
;; Machine-local settings belong in `custom-local.el' next to this file;
;; it is loaded last and is deliberately not tracked in Git.

;;; Code:

;; ============================================================
;;; パッケージ
;; ============================================================
(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/"))
      package-install-upgrade-built-in t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

;; Every `use-package' form installs its package unless it opts out with
;; `:ensure nil', which is how built-in packages are marked below.
(setq use-package-always-ensure t)

;; Customize writes to its own file; it is generated state, not
;; hand-written configuration, so it stays out of Git.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; ============================================================
;;; 基本設定
;; ============================================================
(defun my/keyboard-quit-dwim ()
  "Quit the innermost thing that C-g plausibly refers to.
Without this, C-g inside a completion session leaves the
*Completions* window behind and needs a second press."
  (interactive)
  (cond
   ((region-active-p) (keyboard-quit))
   ((derived-mode-p 'completion-list-mode) (delete-completion-window))
   ((> (minibuffer-depth) 0) (abort-recursive-edit))
   (t (keyboard-quit))))

(use-package emacs
  :ensure nil
  :custom
  (current-language-environment "English")

  ;; 描画・動作
  (bidi-display-reordering nil)
  (ring-bell-function #'ignore)
  (use-short-answers t)

  ;; ファイル操作
  (require-final-newline t)
  (next-line-add-newlines t)
  (make-backup-files nil)
  (create-lockfiles nil)
  (auto-save-default nil)
  (delete-auto-save-files t)
  (delete-by-moving-to-trash t)
  (save-interprogram-paste-before-kill t)

  ;; タブ・インデント
  (indent-tabs-mode nil)
  (tab-width 2)

  ;; 表示
  (line-spacing 4)

  ;; 履歴・ログ
  (history-length 1000)
  (history-delete-duplicates t)
  (set-mark-command-repeat-pop t)
  (message-log-max 10000)

  ;; undo
  (undo-limit 80000000)
  (undo-strong-limit 120000000)
  (undo-outer-limit 360000000)

  ;; whitespace
  (whitespace-style '(face trailing lines-tail))
  (whitespace-line-column 120)

  :config
  (prefer-coding-system 'utf-8)

  (show-paren-mode +1)
  (global-auto-revert-mode +1)
  (global-hl-line-mode +1)
  (global-display-line-numbers-mode +1)
  (pixel-scroll-precision-mode +1)
  (delete-selection-mode +1)
  (line-number-mode +1)
  (column-number-mode +1)
  (tooltip-mode -1)

  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  :bind
  (("RET" . newline-and-indent)
   ("C-j" . newline-and-indent)
   ("C-g" . my/keyboard-quit-dwim)
   ([remap list-buffers] . ibuffer)))

;; *Warnings* と *Compile-Log* が勝手にウィンドウを奪うのを防ぐ。
;; バッファ自体は残るので、内容は C-x b で確認できる。
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; 行番号が邪魔になるバッファでは無効化する。
(dolist (hook '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'whitespace-cleanup nil t)))


;; ============================================================
;;; 組み込みパッケージ
;; ============================================================
(use-package saveplace
  :ensure nil
  :init (save-place-mode 1))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 10)
  :config (recentf-mode 1))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "[^*]+"))

(use-package ediff
  :ensure nil
  :custom
  (ediff-split-window-function #'split-window-horizontally))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-separator "  ")
  :config
  (tab-bar-mode +1))

;; ============================================================
;;; GC
;; ============================================================
(use-package gcmh
  :demand t
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold (* 100 1024 1024))
  :config (gcmh-mode 1))

;; ============================================================
;;; 実行パス
;; ============================================================
(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize))
  ;; GUI から起動した場合にログインシェルの PATH が届かないことがあるため、
  ;; 常に使うディレクトリだけは明示的に足しておく。
  (dolist (dir (list (expand-file-name "~/.local/bin")
                     (expand-file-name "~/.cargo/bin")))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir ":" (getenv "PATH"))))))

;; ============================================================
;;; 外観
;; ============================================================
(use-package nerd-icons)

(use-package emojify
  :hook ((org-mode markdown-mode) . emojify-mode))

(use-package doom-themes
  :if (display-graphic-p)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;; テーマ読み込み後に上書きされないよう、タブバーの face はここで設定する。
  (set-face-attribute 'tab-bar nil
                      :background "#282c34"
                      :foreground "#bbc2cf"
                      :height 1.0)
  (set-face-attribute 'tab-bar-tab nil
                      :inherit 'tab-bar
                      :background "#51afef"
                      :foreground "#ffffff"
                      :weight 'bold
                      :box '(:line-width (10 . 4) :color "#51afef" :style nil))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :inherit 'tab-bar
                      :box '(:line-width (10 . 4) :color "#282c34" :style nil)))

(use-package doom-modeline
  :if (display-graphic-p)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-height 30)
  (doom-modeline-column-zero-based t)
  :config (doom-modeline-mode 1))

(use-package nyan-mode
  :if (display-graphic-p)
  :after doom-modeline
  :config (nyan-mode 1))

(use-package beacon
  :config (beacon-mode 1))

(use-package yascroll
  :config (global-yascroll-bar-mode 1))

(use-package dashboard
  :custom
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 5) (bookmarks . 5)))
  :config (dashboard-setup-startup-hook))

;; ============================================================
;;; フォント
;; ============================================================
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 120)
  (dolist (charset '(japanese-jisx0208
                     japanese-jisx0212
                     japanese-jisx0213-1
                     japanese-jisx0213-2
                     katakana-jisx0201
                     kana
                     han
                     symbol
                     cjk-misc
                     bopomofo))
    (set-fontset-font t charset (font-spec :family "Noto Sans CJK JP"))))

;; ============================================================
;;; 補完・検索
;; ============================================================
(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 20)
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  ;; 検索・移動系だけ表示形式を変える。M-x や find-file は標準表示のまま。
  ;; ここに (t vertical) は書かない。vertical は関数ではないためエラーになる。
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)
          (consult-grep buffer)
          (consult-line buffer)
          (consult-line-multi buffer)
          (consult-buffer reverse))))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :bind ("C-c v r" . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package marginalia
  :config (marginalia-mode 1))

(use-package orderless
  :demand t
  :custom
  ;; Vertico の M-x 補完で不安定になりやすいので orderless-migemo は使わない。
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion basic)))))

;; ローマ字で日本語を検索する。cmigemo と辞書が入っている環境でのみ有効。
;; Arch Linux では AUR: paru -S cmigemo-git
(defvar my/migemo-dictionary-candidates
  '(;; cmigemo の CMake ビルドが使う既定のパス (Arch の AUR パッケージなど)
    "/usr/share/cmigemo/utf-8/migemo-dict"
    "/usr/local/share/cmigemo/utf-8/migemo-dict"
    ;; Homebrew
    "/opt/homebrew/share/migemo/utf-8/migemo-dict"
    ;; Debian/Ubuntu 系の migemo パッケージ
    "/usr/share/migemo/utf-8/migemo-dict")
  "Places a `migemo-dict' has been observed, in order of preference.
The path differs per distribution, so it is probed rather than assumed.")

(use-package migemo
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  ;; cmigemo か辞書が欠けている環境では静かに無効のままにする。
  (when-let* ((dict (seq-find #'file-readable-p my/migemo-dictionary-candidates))
              ((executable-find migemo-command)))
    (setq migemo-dictionary dict)
    (migemo-init)))

(defun my/consult-line (&optional at-point)
  "Search the buffer with `consult-line'.
With a prefix argument AT-POINT, seed the search with the symbol at point."
  (interactive "P")
  (if at-point
      (consult-line (thing-at-point 'symbol))
    (consult-line)))

(use-package consult
  :demand t
  :bind (("C-s"   . my/consult-line)
         ("C-M-s" . consult-line-multi)
         ;; C-x C-f は標準の find-file のまま。
         ;; fzf 的なファイル検索は M-g f / C-c s f を使う。
         ("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ("M-g l" . consult-line)
         ("M-g r" . consult-ripgrep)
         ("M-g f" . consult-find)
         ;; 「探す」系をまとめた入口
         ("C-c s r" . consult-ripgrep)
         ("C-c s f" . consult-find)
         ("C-c s l" . consult-line)
         ("C-c s b" . consult-buffer)
         ("C-c s m" . consult-line-multi))
  :custom
  ;; fd / ripgrep を backend に使う。Arch Linux では: sudo pacman -S fd ripgrep
  (consult-find-command
   "fd --color=never --full-path --hidden --exclude .git --exclude node_modules --exclude .next --exclude target --exclude dist --exclude vendor . ARG OPTS")
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden -g !.git -g !node_modules -g !.next -g !target -g !dist -g !vendor .")
  )

(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-c e" . embark-act)
         ("C-c ;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================
;;; 入力補完 (Corfu / CAPF)
;; ============================================================
(defun my/corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer unless another completion UI owns it."
  (unless (or (bound-and-true-p vertico--input)
              (bound-and-true-p mct--active))
    (corfu-mode 1)))

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary nil)
  (corfu-auto-delay 0.2)
  (corfu-scroll-margin 2)
  :bind (:map corfu-map
              ("TAB"        . corfu-next)
              ("<tab>"      . corfu-next)
              ("S-TAB"      . corfu-previous)
              ("<backtab>"  . corfu-previous)
              ("RET"        . corfu-insert)
              ("<return>"   . corfu-insert))
  :hook (minibuffer-setup . my/corfu-enable-in-minibuffer)
  :config
  (global-corfu-mode 1))

(use-package cape
  :demand t
  :bind ("M-/" . completion-at-point)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; ============================================================
;;; 編集支援
;; ============================================================
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package evil
  :custom
  ;; 既定は Emacs state。Vim 操作は明示的に切り替えたときだけ使う。
  (evil-default-state 'emacs)
  :config (evil-mode 1))

(use-package evil-smartparens
  :after (evil smartparens)
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package vundo
  :bind (("C-x u"   . vundo)
         ("C-c u v" . vundo)))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package bm
  :bind (("M-\\" . bm-toggle)
         ("M-["  . bm-previous)
         ("M-]"  . bm-next)))

(use-package yafolding
  :hook (prog-mode . yafolding-mode)
  :bind (("M-S-<return>" . yafolding-go-parent-element)
         ("M-<return>"   . yafolding-toggle-element)))

(use-package scratch-pop
  :bind ("C-c s" . scratch-pop)
  :custom
  (scratch-pop-backup-directory (locate-user-emacs-file "scratch-pop/"))
  :config
  (add-hook 'kill-emacs-hook #'scratch-pop-backup-scratches))

;; ============================================================
;;; Git
;; ============================================================
(use-package magit
  :bind ("C-x g" . magit-status))

;; ============================================================
;;; Tree-sitter
;; ============================================================
(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

;; ============================================================
;;; LSP (Eglot)
;; ============================================================
(use-package eglot
  :ensure nil
  :hook ((c-mode c-ts-mode
          js-mode js-ts-mode
          typescript-ts-mode tsx-ts-mode
          python-mode python-ts-mode
          rust-mode rust-ts-mode
          ruby-mode ruby-ts-mode
          kotlin-mode)
         . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-g s" . consult-eglot-symbols)))

;; ============================================================
;;; 言語別設定
;; ============================================================
(use-package rust-mode
  :custom
  (rust-indent-offset 4)
  (rust-format-on-save t)
  (rust-format-show-buffer nil))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.scss\\'"  . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.twig\\'"  . web-mode)
         ("\\.vue\\'"   . web-mode)
         ("\\.jsx\\'"   . web-mode)
         ("\\.tsx\\'"   . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-style-padding 1)
  (web-mode-script-padding 1))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package swift-mode :mode "\\.swift\\'")

(use-package dockerfile-mode)
(use-package terraform-mode)
(use-package kotlin-mode)
(use-package json-mode)
(use-package yaml-mode)
(use-package vcl-mode)
(use-package lua-mode)

;; ============================================================
;;; マシン固有設定
;; ============================================================
;; Git 管理外。ホスト固有のパスや資格情報はこちらに書く。
(load (locate-user-emacs-file "custom-local.el") :no-error-if-file-is-missing)

(provide 'init)
;;; init.el ends here
