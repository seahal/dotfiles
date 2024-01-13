;;; IMEパッチ for OSX, https://github.com/takaxp/ns-inline-patch
(when (and (memq window-system '(ns nil))
           (fboundp 'mac-get-current-input-source))
  (when (version< "27.0" emacs-version)
    (custom-set-variables
     '(mac-default-input-source "com.google.inputmethod.Japanese.base")))
  (mac-input-method-mode 1))



;;; Leaf
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


;;; Leaf の設定
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)
(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))



; <<< Preferences >>>
(set-language-environment 'Japanese) ; 言語
(prefer-coding-system 'utf-8) ; utf-8 を使いたい
(savehist-mode +1) ;; コマンド履歴を保存
(save-place-mode +1) ;; 最後のカーソル位置を記録
(recentf-mode +1) ;; ファイルの閲覧履歴を保存
(show-paren-mode +1) ;; 対応括弧を強調表示（Emacs28から標準でON）
(global-auto-revert-mode +1) ;; 他ファイルの変更を常に反映する
(global-hl-line-mode +1) ;; 現在行を強調
(global-display-line-numbers-mode +1) ;; 左側に行番号を表示する
(electric-pair-mode +1) ;; 括弧を補完する
(pixel-scroll-mode +1) ;; マウスホイールのスクロール幅を一般的なものに変更
(menu-bar-mode -1) ;; メニューバーを無効
(tool-bar-mode -1) ;; ツールバーを無効
(scroll-bar-mode -1) ;; スクロールバーを無効
(global-whitespace-mode +1) ; 末尾のスペースを可視化
(gcmh-mode +1) ; emacs のガベージコレクション
(setq confirm-kill-emacs 'y-or-n-p) ;; C-x C-c で閉じる時に、ワンクッション置く
;;; Back up file をつくらせない
(setq make-backup-files nil) ; 余計なファイルを生成させない
(setq auto-save-default nil) ; 自動保存を使用しない
(setq auto-save-list-file-prefix nil) ; ~ が最後につくファイルを作らせない
;;; quiet startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
;; Tab preferences
(tab-bar-mode +1) ; 最初からタブをだす
(global-set-key (kbd "s-t") 'tab-new)
(global-set-key (kbd "s-w") 'tab-close)
(global-set-key (kbd "s-}") 'tab-next)
(global-set-key (kbd "s-{") 'tab-previous)




(leaf exec-path-from-shell
  :ensure t
  :hook (after-init-hook . exec-path-from-shell-initialize))



;; font
(setq-default line-spacing 8)
(leaf nerd-icons :ensure t)
(leaf all-the-icons :ensure t)



;; Theme
(leaf doom-themes
  :if (display-graphic-p)
  :ensure t
  :custom
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . nil)
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))



(leaf doom-modeline
  :if (display-graphic-p)
  :ensure t
  :config
  (doom-modeline-mode 1))


(leaf nyan-mode
  :if (display-graphic-p)
  :after doom-modeline
  :ensure t
  :config
  (nyan-mode 1))



(leaf dashboard
  :doc "emacs startup screen"
  :ensure t
  :config
  (dashboard-setup-startup-hook))



(leaf vertico
  :ensure t
  :config
  (vertico-mode +1)
  (vertico-buffer-mode t))



(leaf corfu
  :ensure t)



(leaf eglot
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'ruby-mode-hook 'eglot-ensure))



(leaf yasnippet
  :ensure t
  :hook (after-init-hook . yas-global-mode))



(leaf undohist
  :ensure t
  :hook (after-init-hook . undohist-initialize))



(leaf undo-tree
  :doc '樹状に履歴を残してくれるやつ'
  :ensure t
  :bind ("C-x u" . undo-tree-visualize)
  :config (undo-tree-mode t))



(leaf point-undo
  :el-get "emacsmirror/point-undo"
  :bind
  ("s-<" . point-undo)
  ("s->" . point-redo))



(leaf bm
  :ensure t
  :bind
  (("s-\\" . bm-toggle)
   ("s-]" . bm-previous)
   ("s-[" . bm-next)))



(leaf yafolding
  :ensure t
  :bind
  (("C-c f" . yafolding-toggle-element))
  :hook (after-init-hook . yafolding-mode))



(leaf rust-mode
  :custom
  (rust-indent-offset . 4)
  (rust-format-on-save . t)
  (rust-format-show-buffer . nil)) ; formatの度にbufferが分割するのを避ける
(leaf cargo
  :ensure t)





;(which-function-mode +1) ;; モードラインにカーソル上の関数名等を表示する
;(desktop-save-mode 1) ; tabの構成を含めて復元できる (実質的な永続化の実現)
