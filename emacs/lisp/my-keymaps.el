;;; my-keymaps.el -*- lexical-binding: t; -*-

;; my-magit-map
(bind-keys :map ctl-x-map
           :prefix-map my-magit-map
           :prefix "g")

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

;; my-go-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-go-map
           :prefix "g")

;; my-org-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-org-map
           :prefix "o")

;; my-tab-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-tab-map
           :prefix "t")

;; my-flymake-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-flymake-map
           :prefix "!")

;; my-window-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-window-map
           :prefix "w")

;; my-toggle-map
(bind-keys :map my-ctl-z-map
           :prefix-map my-toggle-map
           :prefix "~")

(provide 'my-keymaps)
