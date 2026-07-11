;;; my-keymaps.el -*- lexical-binding: t; -*-

(defvar-keymap my-C-z-map
  :doc "My personal keymap avoiding C-c conflicts with package-defined keys.")
(keymap-set global-map "C-z" my-C-z-map)

(dolist (spec '(("4"   my-C-z-4-map   "other window")
                ("5"   my-C-z-5-map   "other frame")
                ("!"   my-C-z-!-map   "flymake")
                ("g"   my-C-z-g-map   "general")
                ("l"   my-C-z-l-map   "lsp")
                ("o"   my-C-z-o-map   "org")
                ("s"   my-C-z-s-map   "surround")
                ("C-t" my-C-z-C-t-map "toggle")
                ("C-w" my-C-z-C-w-map "window")
                ("M-g" my-C-z-M-g-map "magit")))
  (let ((key  (nth 0 spec))
        (map  (nth 1 spec))
        (desc (nth 2 spec)))
    (define-prefix-command map)
    (keymap-set my-C-z-map key (cons desc map))))

(provide 'my-keymaps)
