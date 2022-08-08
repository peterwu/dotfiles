;;; my-dired.el -*- lexical-binding: t; -*-

;; dired
(with-package 'dired
  (with-eval-after-load 'evil
    (setq delete-by-moving-to-trash t)
    (setq dired-dwim-target t)
    (setq dired-listing-switches
          "-AGFhlv --group-directories-first --time-style=long-iso")
    (setq dired-recursive-copies 'always)
    (setq dired-recursive-deletes 'always)

    ;; hooks
    (add-hook 'dired-mode-hook #'dired-hide-details-mode)
    (add-hook 'dired-mode-hook #'hl-line-mode))

  ;; bind keys
  ;; :bind (:map wy:evil-dired-map
  ;;             ("j" . dired-jump)
  ;;             ("J" . dired-jump-other-window)))
  )

;; dired-aux
(with-package 'dired-aux
  (setq dired-create-destination-dirs 'ask)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-vc-rename-file t)

  ;; bind keys
  (define-key dired-mode-map
    (kbd "C-+") #'dired-create-empty-file))

;; dired-sidebar
(with-package 'dired-sidebar
  (setq dired-sidebar-set-width 41)

  ;; bind keys
  (global-set-key (kbd "<f9>") #'dired-sidebar-toggle-sidebar))

;; dired-x
(with-package 'dired-x
  (setq dired-bind-info nil)
  (setq dired-bind-man nil)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-clean-up-buffers-too t)
  (setq dired-guess-shell-alist-user '(("\\.pdf$" "xdg-open * &")))
  (setq dired-x-hands-off-my-keys t)

  ;; bind keys
  (define-key dired-mode-map
    (kbd "I") #'dired-info))

;; image-dired
(with-package 'image-dired
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumbs-per-row 4)

  ;; bind keys
  (define-key image-dired-thumbnail-mode-map
    (kbd "RET") #'image-dired-thumbnail-display-external))

;; wdired
(with-package 'wdired
  (with-eval-after-load 'dired
    (setq wdired-allow-to-change-permissions t)
    (setq wdired-create-parent-directories t))

  ;; autoload
  (autoload-do-load 'wdired-change-to-wdired-mode))

(provide 'my-dired)
