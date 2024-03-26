;;; my-keymaps.el -*- lexical-binding: t; -*-

;; my-jump-map
(define-prefix-command 'my-jump-map)
(global-set-key (kbd "C-c j") my-jump-map)

;; my-magit-map
(define-prefix-command 'my-magit-map)
(global-set-key (kbd "C-c g") my-magit-map)

;; my-org-map
(define-prefix-command 'my-org-map)
(global-set-key (kbd "C-c o") my-org-map)

;; my-toggle-map
(define-prefix-command 'my-toggle-map)
(global-set-key (kbd "C-c t") my-toggle-map)

(provide 'my-keymaps)
