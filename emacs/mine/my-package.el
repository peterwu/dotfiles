;;; my-package.el -*- lexical-binding: t; -*-

(use-package package
  :custom
  (package-archives
   '(("gnu"    . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("stable" . "https://stable.melpa.org/packages/")
     ("melpa"  . "https://melpa.org/packages/")))
  (package-archive-priorities
   '(("gnu"    . 3)
     ("nongnu" . 2)
     ("stable" . 1)
     ("melpa"  . 0))))

(provide 'my-package)
