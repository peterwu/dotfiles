;;; my-keymaps.el -*- lexical-binding: t; -*-

;; my-magit-map
(bind-keys :prefix-map my-magit-map
           :prefix "C-c g")

;; my-jump-map
(bind-keys :prefix-map my-jump-map
           :prefix "C-c j")

;; my-org-map
(bind-keys :prefix-map my-org-map
           :prefix "C-c o")

;; my-surround-map
(bind-keys :prefix-map my-surround-map
           :prefix "C-c s")

;; my-toggle-map
(bind-keys :prefix-map my-toggle-map
           :prefix "C-c t")

;; my-window-map
(bind-keys :prefix-map my-window-map
           :prefix "C-c w")

;; my-mark-map
(bind-keys :prefix-map my-mark-map
           :prefix "C-c SPC")

(provide 'my-keymaps)
