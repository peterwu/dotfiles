;;; my-dired.el -*- lexical-binding: t; -*-

;; dired
(with-package 'dired
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  ;; hooks
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

;; dired-aux
(with-package 'dired-aux
  (setq dired-create-destination-dirs 'ask)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-vc-rename-file t)

  ;; bind keys
  (define-key dired-mode-map
    (kbd "C-+") #'dired-create-empty-file))

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
  (autoload-do-load #'wdired-change-to-wdired-mode))

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
      (my-dired-explorer-show-directory "."))
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

(global-set-key (kbd "<f9>") #'my-dired-explorer-toggle-window)

;; autoload
(autoload-do-load #'my-dired-explorer-toggle-window)

(defvar my-dired-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my-dired-explorer-find-file-at-point)
    (define-key map (kbd "-")   #'my-dired-explorer-find-up-directory)
    (define-key map (kbd "^")   #'my-dired-explorer-find-up-directory)
    map))

;;;###autoload
(define-minor-mode my-dired-explorer-mode
  "Get your foos in the right places."
  :key my-dired-explorer-mode-map

  (with-eval-after-load 'evil
    (evil-define-key 'normal my-dired-explorer-mode-map (kbd "RET") #'my-dired-explorer-find-file-at-point)
    (evil-define-key 'normal my-dired-explorer-mode-map (kbd "-")   #'my-dired-explorer-find-up-directory)
    (evil-define-key 'normal my-dired-explorer-mode-map (kbd "^")   #'my-dired-explorer-find-up-directory)

    (evil-normalize-keymaps)))

(provide 'my-dired)
