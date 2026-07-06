;;; my-keymaps.el -*- lexical-binding: t; -*-

(defvar-keymap my-ctl-z-map
  :doc "My personal keymap avoiding C-c conflicts with package-defined keys.")
(keymap-set global-map "C-z" my-ctl-z-map)

(dolist (spec '(("4"   my-ctl-z-4-map     "other window")
                ("5"   my-ctl-z-5-map     "other frame")
                ("!"   my-ctl-z-!-map     "flymake")
                ("g"   my-ctl-z-g-map     "general")
                ("l"   my-ctl-z-l-map     "lsp")
                ("o"   my-ctl-z-o-map     "org")
                ("s"   my-ctl-z-s-map     "surround")
                ("C-t" my-ctl-z-ctl-t-map "toggle")
                ("C-w" my-ctl-z-ctl-w-map "window")))
  (let ((key  (nth 0 spec))
        (map  (nth 1 spec))
        (desc (nth 2 spec)))
    (define-prefix-command map)
    (keymap-set my-ctl-z-map key (cons desc map))))

(provide 'my-keymaps)
