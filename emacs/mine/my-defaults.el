;;; my-defaults.el -*- lexical-binding: t; -*-

;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default locale-coding-system 'utf-8)

;; global settings
(setopt auto-save-default nil)
(setopt auto-save-list-file-prefix nil)
(setopt bookmark-default-file (expand-file-name
                               "cache/bookmarks"
                               user-emacs-directory))
(setopt confirm-kill-emacs #'yes-or-no-p)
(setopt fill-column 80)
(setopt indent-tabs-mode nil)
(setopt inhibit-startup-buffer-menu t)
(setopt inhibit-startup-echo-area-message (user-login-name))
(setopt inhibit-startup-screen t)
(setopt initial-major-mode 'org-mode)
(setopt initial-scratch-message nil)
(setopt make-backup-files nil)
(setopt nsm-settings-file
              (expand-file-name
               "cache/network-security.data"
               user-emacs-directory))
(setopt scroll-conservatively 10000)
(setopt scroll-margin 1)
(setopt scroll-preserve-screen-position t)
(setopt scroll-step 1)
(setopt select-enable-clipboard nil)
(setopt tab-always-indent 'complete)
(setopt tab-width 4)
(setopt use-dialog-box nil)
(setopt use-file-dialog nil)
(setopt use-short-answers t)
(setopt vc-follow-symlinks t)
(setopt visible-bell t)

(setq-default backup-inhibited t)
(setq-default bidi-inhibit-bpa t)
(setq-default frame-title-format
              '((:eval
                 (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

;; custom.el
(defconst my-custom-file
  (expand-file-name "cache/custom.el" user-emacs-directory))
(setopt custom-file my-custom-file)
(add-hook 'after-init-hook
          (lambda ()
            (let ((file my-custom-file))
              (unless (file-exists-p file)
                (make-empty-file file))
              (load-file file))))

;; blinking
(setopt blink-cursor-blinks 20)
(setopt blink-cursor-delay 0.2)
(setopt blink-cursor-interval 0.5)
(setopt cursor-in-non-selected-windows 'hollow)
(setopt cursor-type '(hbar . 3))
(blink-cursor-mode +1)

;; hl-line
(global-hl-line-mode +1)

;; pixel-scroll-precision-mode
(setopt pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode +1)

;; simple
(column-number-mode +1)
(global-visual-line-mode +1)
(prettify-symbols-mode +1)
(size-indication-mode +1)

;; so-long
(global-so-long-mode +1)

;; minibuffer
(setopt completion-cycle-threshold 3)
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completion-styles '(initials partial-completion flex))
(setopt enable-recursive-minibuffers t)
(setopt minibuffer-eldef-shorten-default t)
(setopt read-buffer-completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)
(setopt resize-mini-windows t)

(minibuffer-depth-indicate-mode +1)
(minibuffer-electric-default-mode +1)

;; hooks
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; key binds
(keymap-set mode-specific-map "RET" #'pp-macroexpand-last-sexp)

(provide 'my-defaults)
