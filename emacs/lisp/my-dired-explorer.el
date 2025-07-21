;;; my-dired-explorer.el -*- lexical-binding: t; -*-

(require 'dired)

(defconst my-dired-explorer-buffer-name "*MY-DIRED-EXPLORER*"
  "The buffer name of my-dired-explorer")

(defvar my-dired-explorer-buffer nil
  "Define my-dired-explorer-buffer")

(defvar my-dired-explorer-window nil
  "Define my-dired-explorer window")

(defconst my-dired-explorer-window-width-in-percentage 30
  "Set the default window width in percentage.")

(defun my-dired-explorer-window-exists-p ()
  "Return `non-nil' if my-dired-explorer window exists.
Otherwise return nil."
  (and my-dired-explorer-window
       (window-live-p my-dired-explorer-window)))

(defun my-dired-explorer-buffer-exists-p ()
  "Return `non-nil' if my-dired-explorer buffer exists.
Otherwise return nil."
  (and my-dired-explorer-buffer
       (buffer-live-p my-dired-explorer-buffer)))

(defun my-dired-explorer-exists-p ()
  "Return `non-nil' if `my-dired-explorer' exists.
Otherwise return nil."
  (and (my-dired-explorer-buffer-exists-p)
       (my-dired-explorer-window-exists-p)))

(defun my-dired-explorer-visit-directory (dir)
  "Visit the directory in my-dired-explorer"
  (when (my-dired-explorer-buffer-exists-p)
    (kill-buffer my-dired-explorer-buffer))
  (setq my-dired-explorer-buffer (dired-noselect dir))
  (set-window-buffer my-dired-explorer-window my-dired-explorer-buffer)

  (with-current-buffer my-dired-explorer-buffer
    (buffer-disable-undo)
    (rename-buffer my-dired-explorer-buffer-name)
    (my-dired-explorer-mode +1)))

(defun my-dired-explorer-find-file ()
  "Open the file at point from dired."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (and (file-directory-p file)
             (file-readable-p file))
        (my-dired-explorer-visit-directory file)
      (find-file-other-window file))))

(defun my-dired-explorer-up-directory ()
  "Run Dired on parent directory of current directory."
  (interactive)
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (my-dired-explorer-visit-directory up)))

;;;###autoload
(defun my-dired-explorer-show ()
  "Show my-dired-explorer window"
  (interactive)
  (unless (my-dired-explorer-exists-p)
    (setq my-dired-explorer-window
          (split-window
           (frame-root-window)
           (- (window-total-width (frame-root-window))
              (truncate (/ (* (window-total-width (frame-root-window))
                              my-dired-explorer-window-width-in-percentage)
                           100)))
           'left))

    (my-dired-explorer-visit-directory default-directory)
    (select-window my-dired-explorer-window)))

;;;###autoload
(defun my-dired-explorer-hide ()
  "Hide my-dired-explorer window"
  (interactive)
  (when (my-dired-explorer-exists-p)
    (let ((current-window (selected-window)))
      (delete-window my-dired-explorer-window)
      (kill-buffer my-dired-explorer-buffer)
      (and current-window
           (window-live-p current-window)
           (select-window current-window)))))

;;;###autoload
(defun my-dired-explorer-toggle ()
  "Toggle my-dired-explorer window"
  (interactive)
  (if (my-dired-explorer-exists-p)
      (my-dired-explorer-hide)
    (my-dired-explorer-show)))

(defvar-local my-dired-explorer-current-directory
    '(:eval
      (let ((dir
             (file-name-nondirectory (directory-file-name default-directory)))
            (face
             (if (mode-line-window-selected-p)
                 '(:inherit mode-line-emphasis)
               '(:inherit mode-line-inactive))))
        (propertize dir
                    'help-echo (abbreviate-file-name default-directory)
                    'face face)))
  "Return the current directory.")
(put 'my-dired-explorer-current-directory 'risky-local-variable t)

(define-minor-mode my-dired-explorer-mode
  "A minor mode for my-dired-explorer"
  :global nil
  :keymap (make-sparse-keymap)
  (when my-dired-explorer-mode
    (set-keymap-parent my-dired-explorer-mode-map dired-mode-map)
    (setq-local mode-line-format
                '(:eval (list
                         my-mode-line-window-status-tag
                         " "
                         my-dired-explorer-current-directory)))
    (bind-keys :map my-dired-explorer-mode-map
               ("^" . my-dired-explorer-up-directory)
               ("-" . my-dired-explorer-up-directory)
               ("q" . my-dired-explorer-hide)
               ("RET" . my-dired-explorer-find-file)
               ("<mouse-1>" . my-dired-explorer-find-file))))

(bind-keys :map my-ctl-z-map
           ("d" . my-dired-explorer-toggle))

(provide 'my-dired-explorer)
