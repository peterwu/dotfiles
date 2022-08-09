;;; my-package.el -*- lexical-binding: t; -*-

(require 'package)

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu"          . 3)
        ("nongnu"       . 2)
        ("melpa-stable" . 1)
        ("melpa"        . 0)))

(provide 'my-package)
