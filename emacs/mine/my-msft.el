;;; my-msft.el -*- lexical-binding: t; -*-

;; special tweaks for Microsoft Windows
(when (eq system-type 'windows-nt)
  (setq set-selection-coding-system 'utf-16le-dos)
  (setq w32-get-true-file-attributes nil)
  (setq inhibit-compacting-font-caches t))

(provide 'my-msft)
