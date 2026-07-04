;;; my-keybinds.el -*- lexical-binding: t; -*-

;; Use C-z for my personal key maps to avoid conflicts with
;; C-c keybinds defined by certain packages
(bind-keys :map global-map
           ("C-z" . nil)

           ("C-x C-c" . nil)
           ("C-x C-c C-c" . save-buffers-kill-terminal)

           :prefix-map my-ctl-z-map
           :prefix "C-z")

(dolist (spec '(("4"   . my-ctl-z-4-map)
                ("5"   . my-ctl-z-5-map)
                ("!"   . my-ctl-z-!-map)
                ("g"   . my-ctl-z-g-map)
                ("l"   . my-ctl-z-l-map)
                ("o"   . my-ctl-z-o-map)
                ("s"   . my-ctl-z-s-map)
                ("t"   . my-ctl-z-t-map)
                ("C-t" . my-ctl-z-ctl-t-map)
                ("C-w" . my-ctl-z-ctl-w-map)))
  (let ((key (car spec))
        (map-name (cdr spec)))
    (define-prefix-command map-name)
    (define-key my-ctl-z-map (kbd key) map-name)))

(provide 'my-keybinds)
