;;; my-platforms.el -*- lexical-binding: t; -*-

(cond
 ((eq system-type 'windows-nt)
  (setopt set-selection-coding-system 'utf-16le-dos)
  (setopt w32-get-true-file-attributes nil)
  (setq-default inhibit-compacting-font-caches t))
 ((eq system-type 'darwin)
  (setopt mac-command-modifier 'meta)
  (setopt mac-option-modifier nil)))

(provide 'my-platforms)
