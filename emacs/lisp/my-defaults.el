;;; my-defaults.el -*- lexical-binding: t; -*-

;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default locale-coding-system 'utf-8)

;; save nothing
(setopt auto-save-default nil)
(setopt auto-save-list-file-prefix nil)
(setopt make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default create-lockfiles nil)

;; help
(setopt help-window-select t)

;; tab
(setopt tab-always-indent 'complete)
(setopt tab-width 4)

(provide 'my-defaults)
