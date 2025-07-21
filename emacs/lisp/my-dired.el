;;; my-dired.el -*- lexical-binding: t; -*-

;; dired
(use-package dired
  :custom
  (delete-by-moving-to-trash t)
  (insert-directory-program "/usr/local/bin/gls")

  (dired-bind-info nil)
  (dired-bind-man nil)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-clean-up-buffers-too t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-guess-shell-alist-user '(("\\.pdf$" "xdg-open * &")))
  (dired-isearch-filenames 'dwim)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches
   "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-mouse-drag-files t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-use-ls-dired t)
  (dired-vc-rename-file t)
  :bind
  (:map dired-mode-map
        ("-"   . dired-up-directory)
        ("C-+" . dired-create-empty-file)
        ("I"   . dired-do-info))
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode))

;; dired-x
(use-package dired-x
  :custom
  (dired-x-hands-off-my-keys t))

;; image-dired
(use-package image-dired
  :custom
  (image-dired-external-viewer "xdg-open")
  (image-dired-thumb-margin 2)
  (image-dired-thumb-relief 0)
  (image-dired-thumb-size 80)
  (image-dired-thumbs-per-row 4)
  :bind
  (:map image-dired-thumbnail-mode-map
        ("RET" . image-dired-thumbnail-display-external)))

;; wdired
(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(provide 'my-dired)
