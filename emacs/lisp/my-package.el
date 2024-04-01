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

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-03/msg00114.html
(add-to-list 'package--builtin-versions '(transient 0 4 3))

(provide 'my-package)
