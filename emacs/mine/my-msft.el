;;; my-msft.el -*- lexical-binding: t; -*-

;; special tweaks for Microsoft Windows
(when (eq system-type 'windows-nt)
  (setopt set-selection-coding-system 'utf-16le-dos)
  (setopt w32-get-true-file-attributes nil)
  (setq-default inhibit-compacting-font-caches t))

(provide 'my-msft)
