;;; my-dired.el -*- lexical-binding: t; -*-

;; dired
(use-package dired
  :preface
  (when (eq system-type 'darwin)
    (setq-default insert-directory-program "gls"))
  :custom
  (dired-bind-info nil)
  (dired-bind-man nil)
  (delete-by-moving-to-trash t)
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
  (dired-vc-rename-file t)
  :bind
  (:map dired-mode-map
        ("-"   . dired-up-directory)
        ("C-+" . dired-create-empty-file)
        ("I"   . dired-info))
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

;; my-dired-explorer
(defvar my-dired-explorer-window nil)
(defvar my-dired-explorer-window-width-in-percentage 30)

(defvar-local my-dired-explorer-mode-line-directory-identification
  '(:eval (propertize (concat ":" (my-ellipsize-file-name
                                   (abbreviate-file-name default-directory)
                                   32))
                      'face '(:inherit mode-line-buffer-id))))
(put 'my-dired-explorer-mode-line-directory-identification 'risky-local-variable t)

(defun my-dired-explorer-show-directory (dir)
  (interactive)
  (dired dir)

  (my-dired-explorer-mode +1)

  (setq-local mode-line-format
              '(:eval
                '("%e"
                  evil-mode-line-tag
                  " "
                  my-dired-explorer-mode-line-directory-identification)))
  (force-mode-line-update)

  (dired-advertise))

(defun my-dired-explorer-find-file-at-point ()
  (interactive)
  (with-selected-window my-dired-explorer-window
    (let ((buffer (current-buffer))
          (file (dired-get-file-for-visit)))
      (my-dired-explorer-find-file file)
      (when (file-directory-p file)
        (kill-buffer buffer)))))

(defun my-dired-explorer-mouse-find-file-at-point (event)
  (interactive "e")
  (with-selected-window my-dired-explorer-window
    (let ((pos (posn-point (event-end event))))
      (goto-char pos)
      (call-interactively #'my-dired-explorer-find-file-at-point))))

(defun my-dired-explorer-find-up-directory ()
  (interactive)
  (with-selected-window my-dired-explorer-window
    (let ((buffer (current-buffer))
          (dir (dired-current-directory)))
      (dired-up-directory)
      (unless (eq dir (dired-current-directory))
        (my-dired-explorer-find-file (dired-current-directory))
        (kill-buffer buffer)))))

(defun my-dired-explorer-find-file (file)
  (interactive)
  (if (file-directory-p file)
      (my-dired-explorer-show-directory file)
    (call-interactively #'dired-find-file-other-window)))

(defun my-dired-explorer-show-window ()
  (interactive)
  (unless (window-live-p my-dired-explorer-window)
    (setq my-dired-explorer-window
          (split-window (frame-root-window)
                        (- (window-total-width (frame-root-window))
                           (truncate (/ (* (window-total-width (frame-root-window))
                                           my-dired-explorer-window-width-in-percentage)
                                        100)))
                        'left))

    (with-selected-window my-dired-explorer-window
      (my-dired-explorer-show-directory default-directory)
      (set-window-dedicated-p (selected-window) t))
    (other-window 1)))

(defun my-dired-explorer-hide-window ()
  (interactive)
  (when (window-live-p my-dired-explorer-window)
    (with-selected-window my-dired-explorer-window
      (kill-buffer-and-window))
    (setq my-dired-explorer-window nil)))

;;;###autoload
(defun my-dired-explorer-toggle-window ()
  (interactive)
  (if (window-live-p my-dired-explorer-window)
      (my-dired-explorer-hide-window)
    (my-dired-explorer-show-window)))

(bind-key [f9] #'my-dired-explorer-toggle-window)

;; autoload
(autoload-do-load #'my-dired-explorer-toggle-window)

;;;###autoload
(define-minor-mode my-dired-explorer-mode
  "Get your foos in the right places."
  :keymap (define-keymap
            "<remap> <find-file>" #'my-dired-explorer-find-file-at-point
            "<remap> <find-file-other-window>" #'my-dired-explorer-find-file-at-point
            "<remap> <dired-find-file>" #'my-dired-explorer-find-file-at-point
            "<remap> <dired-find-file-other-window>" #'my-dired-explorer-find-file-at-point
            "<remap> <dired-mouse-find-file>" #'my-dired-explorer-mouse-find-file-at-point
            "<remap> <dired-mouse-find-file-other-window>" #'my-dired-explorer-mouse-find-file-at-point
            "<remap> <dired-up-directory>" #'my-dired-explorer-find-up-directory
            "C-x C-f" (lambda nil (interactive)
                        (call-interactively #'find-file)))

  (dired-hide-details-mode +1))

(defun my-dired-explorer-advice (orig-fn &rest args)
  (when (window-live-p my-dired-explorer-window)
    (set-window-dedicated-p my-dired-explorer-window nil))
  (apply orig-fn args)
  (when (window-live-p my-dired-explorer-window)
    (set-window-dedicated-p my-dired-explorer-window t)))

(advice-add #'my-dired-explorer-find-file-at-point
            :around #'my-dired-explorer-advice)
(advice-add #'my-dired-explorer-find-up-directory
            :around #'my-dired-explorer-advice)

(provide 'my-dired)
