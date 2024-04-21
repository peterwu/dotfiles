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

;; my-delete-map
(bind-keys :prefix-map my-delete-map
           :prefix "C-c d")

;; my-kill-map
;; kill-to-ring
(bind-keys :prefix-map my-kill-map
           :prefix "C-c k")

;; my-KILL-map
;; kill-to-clipboard
(bind-keys :prefix-map my-KILL-map
           :prefix "C-c M-k")

;; my-yank-map
;; yank-to-ring
(bind-keys :prefix-map my-yank-map
           :prefix "C-c y")

;; my-YANK-map
;; yank-to-clipboard
(bind-keys :prefix-map my-YANK-map
           :prefix "C-c M-y")

(provide 'my-keymaps)
