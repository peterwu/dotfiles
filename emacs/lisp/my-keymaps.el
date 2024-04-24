;;; my-keymaps.el -*- lexical-binding: t; -*-

;; Use C-z for my personal key maps to avoid conflicts with
;; C-c keybinds defined by certain packages
(bind-keys :map global-map
           ("C-z" . nil)
           :prefix-map my-ctl-z-map
           :prefix "C-z")

;; my-magit-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-magit-map
           :prefix "g")

;; my-org-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-org-map
           :prefix "o")

;; my-toggle-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-toggle-map
           :prefix "t")

;; my-flymake-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-flymake-map
           :prefix "!")

;; my-window-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-window-map
           :prefix "w")

;; my-mark-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-mark-map
           :prefix "SPC")

;; my-delete-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-delete-map
           :prefix "d")

;; my-kill-map (kill-to-ring)
(bind-keys :map my-ctl-z-map
           :prefix-map my-kill-map
           :prefix "k")

;; my-KILL-map (kill-to-clipboard)
(bind-keys :map my-ctl-z-map
           :prefix-map my-KILL-map
           :prefix "M-k")

;; my-yank-map (yank-to-ring)
(bind-keys :map my-ctl-z-map
           :prefix-map my-yank-map
           :prefix "y")

;; my-YANK-map (yank-to-clipboard)
(bind-keys :map my-ctl-z-map
           :prefix-map my-YANK-map
           :prefix "M-y")

;; my-surround-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-surround-map
           :prefix "s")

(provide 'my-keymaps)
