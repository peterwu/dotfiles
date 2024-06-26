;;; my-speedbar.el -*- lexical-binding: t; -*-

(require 'speedbar)

(defconst my-speedbar-buffer-name "*MY-SPEEDBAR*"
  "The buffer name of my-speedbar")

(defvar my-speedbar-window nil
  "Define my-speedbar window")

(defconst my-speedbar-window-width-in-percentage 30
  "Set the default window width in percentage.")

(defvar-local my-speedbar-window-status-tag
    '(:eval
      (modus-themes-with-colors
        (if (mode-line-window-selected-p)
            (propertize " X "
                        'face
                        `(:background ,rust :foreground ,white :weight bold))
          (propertize (format " %i " (my-window-numbering-get-number))
                      'face '(:inverse-video t)))))
  "Return an ellipsized file name when applicable.")
(put 'my-speedbar-window-status-tag 'risky-local-variable t)

(defun my-speedbar-set-mode-line-format-advice ()
  "Override the default mode-line-format."
  (setq-local mode-line-format
              '(:eval my-speedbar-window-status-tag))
  (force-mode-line-update))

(advice-add #'speedbar-set-mode-line-format
            :override #'my-speedbar-set-mode-line-format-advice)

(defun my-speedbar-toggle-show-all-files-advice ()
  "Toggle the appearance of level 2 hidden files."
  (if speedbar-show-unknown-files
      (setq-local speedbar-directory-unshown-regexp "^\\(\\..*\\)\\'")
    (setq-local speedbar-directory-unshown-regexp "^\(\.\.*$\)\'")))

(advice-add #'speedbar-toggle-show-all-files
            :before #'my-speedbar-toggle-show-all-files-advice)

;;;###autoload
(defun my-speedbar-toggle ()
  "Toggle my-speedbar window"
  (interactive)
  (if (my-speedbar-exists-p)
      (my-speedbar-hide)
    (my-speedbar-show)))

;;;###autoload
(defun my-speedbar-show ()
  "Show my-speedbar window"
  (interactive)
  (unless (my-speedbar-exists-p)
    (setq my-speedbar-window
          (split-window
           (frame-root-window)
           (- (window-total-width (frame-root-window))
              (truncate (/ (* (window-total-width (frame-root-window))
                              my-speedbar-window-width-in-percentage)
                           100)))
           'left))
    (setq speedbar-buffer (generate-new-buffer my-speedbar-buffer-name)
          speedbar-frame (selected-frame)
          dframe-attached-frame (selected-frame)
          speedbar-select-frame-method 'attached
          speedbar-verbosity-level 0
          speedbar-last-selected-file nil)
    (set-buffer speedbar-buffer)
    (buffer-disable-undo speedbar-buffer)
    (speedbar-mode)
    (speedbar-reconfigure-keymaps)
    (speedbar-update-contents)

    (set-window-buffer my-speedbar-window speedbar-buffer)
    (set-window-dedicated-p my-speedbar-window t)
    (select-window my-speedbar-window)))

;;;###autoload
(defun my-speedbar-hide ()
  "Hide my-speedbar window"
  (interactive)
  (when (my-speedbar-exists-p)
    (let ((current-window (selected-window)))
      (delete-window my-speedbar-window)
      (and current-window
           (window-live-p current-window)
           (select-window current-window)))))

(defun my-speedbar--kill-buffer-on-deleted-window ()
  "Kill my-speedbar buffer after my-speedbar window is deleted."
  (when (and (my-speedbar-buffer-exists-p)
             (not (my-speedbar-window-exists-p)))

    (kill-buffer my-speedbar-buffer-name)

    (setq my-speedbar-window nil)
    (setq speedbar-buffer nil
          speedbar-frame nil
          dframe-attached-frame nil
          speedbar-last-selected-file nil)))

(add-hook 'window-configuration-change-hook
          #'my-speedbar--kill-buffer-on-deleted-window)

(defun my-speedbar-exists-p ()
  "Return `non-nil' if `my-speedbar' exists.
Otherwise return nil."
  (and (my-speedbar-buffer-exists-p)
       (my-speedbar-window-exists-p)))

(defun my-speedbar-window-exists-p ()
  "Return `non-nil' if my-speedbar window exists.
Otherwise return nil."
  (and my-speedbar-window
       (window-live-p my-speedbar-window)))

(defun my-speedbar-buffer-exists-p ()
  "Return `non-nil' if my-speedbar buffer exists.
Otherwise return nil."
  (and speedbar-buffer
       (buffer-live-p speedbar-buffer)))

(bind-keys :map speedbar-mode-map
           ("^" . speedbar-up-directory)
           ("." . speedbar-toggle-show-all-files)
           ("q" . my-speedbar-hide)
           :map global-map
           ([f9] . my-speedbar-toggle)
           :map my-ctl-z-t-map
           ("x" . my-speedbar-toggle)
           :repeat-map my-speedbar-repeat-map
           ("x" . my-speedbar-toggle))

(provide 'my-speedbar)
