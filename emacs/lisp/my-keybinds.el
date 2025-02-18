;;; my-keybinds.el -*- lexical-binding: t; -*-

;; Use C-z for my personal key maps to avoid conflicts with
;; C-c keybinds defined by certain packages
(bind-keys :map global-map
           ("C-z" . nil)
           :prefix-map my-ctl-z-map
           :prefix "C-z")

;; my-ctl-z-4-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-4-map
           :prefix "4")

;; my-ctl-z-5-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-5-map
           :prefix "5")

;; my-ctl-z-!-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-!-map
           :prefix "!")

;; my-ctl-z-g-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-g-map
           :prefix "g")

;; my-ctl-z-l-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-l-map
           :prefix "l")

;; my-ctl-z-o-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-o-map
           :prefix "o")

;; my-ctl-z-t-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-t-map
           :prefix "t")

;; my-ctl-z-ctl-w-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-ctl-z-ctl-w-map
           :prefix "C-w")

;; avoid quiting Emacs accidentally
(bind-keys :map global-map
           ("C-x C-c" . nil)
           ("C-x C-c C-c" . save-buffers-kill-terminal))

;; clipboard keybinds
(bind-keys :map my-ctl-z-map
           ("M-d" . clipboard-kill-region)
           ("M-y" . clipboard-kill-ring-save)
           ("M-p" . clipboard-yank))

(provide 'my-keybinds)
