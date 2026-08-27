;;; early-init.el --- Loaded before the package system and UI  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs reads this file before `package-initialize' runs and before the
;; first frame is created.  Only settings that must take effect that
;; early belong here; everything else lives in init.el.

;;; Code:

;; Raise the garbage collection threshold for the duration of startup.
;; `gcmh' takes over with sensible steady-state values in init.el.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Consulting file name handlers for every file loaded during startup is
;; pure overhead; nothing loaded here is compressed or remote.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist)))

;; init.el drives package.el explicitly, so skip the implicit activation.
(setq package-enable-at-startup nil)

;; Disable the chrome through frame parameters rather than through the
;; minor modes, so it is never drawn in the first place.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message ""
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(provide 'early-init)
;;; early-init.el ends here
