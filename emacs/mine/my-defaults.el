;;; my-defaults.el -*- lexical-binding: t; -*-

;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; global settings
(setq-default auto-save-default nil)
(setq-default auto-save-list-file-prefix nil)
(setq-default backup-inhibited t)
(setq-default bidi-inhibit-bpa t)
(setq-default confirm-kill-emacs #'yes-or-no-p)
(setq-default fill-column 80)
(setq-default frame-title-format
              '((:eval
                 (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
(setq-default indent-tabs-mode nil)
(setq-default inhibit-startup-buffer-menu t)
(setq-default inhibit-startup-echo-area-message t)
(setq-default inhibit-startup-screen t)
(setq-default initial-major-mode 'org-mode)
(setq-default initial-scratch-message nil)
(setq-default locale-coding-system 'utf-8)
(setq-default make-backup-files nil)
(setq-default nsm-settings-file (expand-file-name "cache/network-security.data" user-emacs-directory))
(setq-default scroll-conservatively 10000)
(setq-default scroll-margin 1)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-step 1)
(setq-default select-enable-clipboard nil)
(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)
(setq-default use-dialog-box nil)
(setq-default use-file-dialog nil)
(setq-default use-short-answers t)
(setq-default vc-follow-symlinks t)
(setq-default visible-bell t)

;; custom.el
(defvar my-custom-file (expand-file-name "cache/custom.el" user-emacs-directory))
(setq-default custom-file my-custom-file)
(add-hook 'after-init-hook (lambda ()
                             (let ((file my-custom-file))
                               (unless (file-exists-p file)
                                 (make-empty-file file))
                               (load-file file))))

;; blinking
(setq-default blink-cursor-blinks 20)
(setq-default blink-cursor-delay 0.2)
(setq-default blink-cursor-interval 0.5)
(setq-default cursor-in-non-selected-windows 'hollow)
(setq-default cursor-type '(hbar . 3))
(blink-cursor-mode +1)

;; hl-line
(global-hl-line-mode +1)

;; simple
(column-number-mode +1)
(global-visual-line-mode +1)
(prettify-symbols-mode +1)
(size-indication-mode +1)

;; so-long
(global-so-long-mode +1)

;; minibuffer
(setq-default completion-cycle-threshold 3)
(setq-default completions-detailed t)
(setq-default completions-format 'one-column)
(setq-default completion-ignore-case t)
(setq-default completion-styles '(initials partial-completion flex))
(setq-default enable-recursive-minibuffers t)
(setq-default minibuffer-eldef-shorten-default t)
(setq-default read-buffer-completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default resize-mini-windows t)

(minibuffer-depth-indicate-mode +1)
(minibuffer-electric-default-mode +1)

;; hooks
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; key binds
(define-key mode-specific-map (kbd "RET") #'pp-macroexpand-last-sexp)

(provide 'my-defaults)
