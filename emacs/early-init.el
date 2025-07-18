;;; early-init.el -*- lexical-binding: t; -*-
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)

;; tune gc for better performance
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original
                  gc-cons-threshold (* 8 1024 1024) ; 8MB
                  gc-cons-percentage 0.1)
            (garbage-collect) t))
(run-with-idle-timer 5 t 'garbage-collect)

;; do not resize the frame at this early stage.
(setopt frame-inhibit-implied-resize t)

;; prefer loading newer compiled files
(setopt load-prefer-newer t )

;; no litterring
(make-directory (expand-file-name "cache" user-emacs-directory) t)

(with-no-warnings ; some of these variables may not have been defined yet

  (setopt bookmark-default-file (expand-file-name
                                 "cache/bookmarks"
                                 user-emacs-directory))

  (setopt ido-save-directory-list-file
          (expand-file-name "cache/ido.last" user-emacs-directory))

  (setopt nsm-settings-file
          (expand-file-name
           "cache/network-security.data"
           user-emacs-directory))

  (setopt package-quickstart t
          package-quickstart-file (expand-file-name
                                   "cache/package-quickstart.el"
                                   user-emacs-directory))

  (setopt project-list-file
          (expand-file-name "cache/projects" user-emacs-directory))

  (setopt recentf-save-file
          (expand-file-name "cache/recentf" user-emacs-directory))

  (setopt save-place-file
          (expand-file-name "cache/places" user-emacs-directory))

  (setopt savehist-file
          (expand-file-name "cache/savehist" user-emacs-directory))

  (setopt tramp-persistency-file-name
          (expand-file-name "cache/tramp" user-emacs-directory)))

;; packages
(setopt package-archives
        '(("gnu"    . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("stable" . "https://stable.melpa.org/packages/")
          ("melpa"  . "https://melpa.org/packages/")))

(setopt package-archive-priorities
        '(("gnu"    . 3)
          ("nongnu" . 2)
          ("stable" . 1)
          ("melpa"  . 0)))

;; custom.el
(setopt custom-file (make-temp-file "emacs-custom-" nil ".el"))

;; clean GUI
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'context-menu-mode)
  (context-menu-mode +1))

;; make native compilation silent
(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors 'silent))

(set-face-attribute 'default        nil :family "SF Mono" :height 160 :weight 'normal)
(set-face-attribute 'fixed-pitch    nil :family "SF Mono" :height 160 :weight 'normal)
(set-face-attribute 'variable-pitch nil :family "SF Pro"  :height 160 :weight 'normal)

;; show nothing at startup
(setopt inhibit-startup-buffer-menu t
        inhibit-startup-echo-area-message user-login-name
        inhibit-startup-screen t
        inhibit-x-resources t)
(setopt initial-buffer-choice nil
        initial-major-mode 'org-mode
        initial-scratch-message nil)

;; zen style experience
(setopt use-dialog-box nil)
(setopt use-file-dialog nil)
(setopt use-short-answers t)
(setopt visible-bell t)

;; disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; faster redisplay
(setq bidi-inhibit-bpa t)

;; start maximized (cross-platform)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; security
(setq gnutls-verify-error t)
(setq tls-checktrust t)
(setq gnutls-min-prime-bits 3072)

(provide 'early-init)
;;; early-init.el ends here
