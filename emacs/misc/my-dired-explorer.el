;;; my-dired-explorer.el -*- lexical-binding: t; -*-

;; A minimal dired-based file explorer shown in a dedicated left side
;; window.  Directories open in place; files open in the main window.
;;
;; Study notes
;; -----------
;; Two ideas make this more robust than a naive `split-window' sidebar:
;;
;;   1. It lives in a *side window* (`display-buffer-in-side-window'), so
;;      it survives `delete-other-windows' (C-x 1) and `other-window'
;;      skips it (`no-other-window').
;;
;;   2. Its dired buffer is *private*: we bind `dired-buffers' to nil
;;      around `dired-noselect' so dired builds a fresh, unregistered
;;      buffer instead of handing back (and letting us clobber) a normal
;;      dired buffer that happens to visit the same directory.
;;
;; The auto-follow section at the bottom is an opt-in "hack" -- see the
;; caveats documented there.

(require 'dired)

(defconst my-dired-explorer-buffer-name "*dired-explorer*"
  "Name of the explorer buffer.")

(defcustom my-dired-explorer-width 30
  "Width of the explorer window, in columns."
  :type 'integer
  :group 'dired)

(defvar my-dired-explorer--buffer nil
  "The explorer's current dired buffer.")

(defun my-dired-explorer--window ()
  "Return the live explorer window, or nil."
  (and (buffer-live-p my-dired-explorer--buffer)
       (get-buffer-window my-dired-explorer--buffer)))

(defun my-dired-explorer--main-window ()
  "Return a window suitable for visiting files, i.e. not a side window."
  (seq-find (lambda (window)
              (not (window-parameter window 'window-side)))
            (window-list nil 'no-minibuffer)))

(defun my-dired-explorer--visit (dir)
  "Show DIR in the explorer, replacing any previous explorer buffer.
Return the explorer window."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (when (buffer-live-p my-dired-explorer--buffer)
      (kill-buffer my-dired-explorer--buffer))
    ;; Bind `dired-buffers' to nil so dired hands us a fresh, unregistered
    ;; buffer instead of hijacking an existing dired buffer for DIR.
    (setq my-dired-explorer--buffer
          (let ((dired-buffers nil))
            (dired-noselect dir)))
    (with-current-buffer my-dired-explorer--buffer
      (rename-buffer my-dired-explorer-buffer-name)
      (buffer-disable-undo)
      (my-dired-explorer-mode +1))
    (let ((window (display-buffer-in-side-window
                   my-dired-explorer--buffer
                   `((side . left)
                     (slot . 0)
                     (window-width . ,my-dired-explorer-width)
                     (preserve-size . (t . nil))
                     (window-parameters
                      . ((no-delete-other-windows . t)
                         (no-other-window . t)))))))
      (set-window-dedicated-p window t)
      window)))

(defun my-dired-explorer-find-file ()
  "Open the item at point.
Directories open in the explorer; files open in the main window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (my-dired-explorer--visit file)
      (if-let* ((window (my-dired-explorer--main-window)))
          (progn (select-window window)
                 (find-file file))
        (find-file-other-window file)))))

(defun my-dired-explorer-up-directory ()
  "Move the explorer to the parent directory."
  (interactive)
  (my-dired-explorer--visit
   (file-name-directory (directory-file-name default-directory))))

;;;###autoload
(defun my-dired-explorer-show ()
  "Show the explorer and select it."
  (interactive)
  (let ((dir (if (buffer-live-p my-dired-explorer--buffer)
                 (buffer-local-value 'default-directory my-dired-explorer--buffer)
               default-directory)))
    (select-window (my-dired-explorer--visit dir))))

;;;###autoload
(defun my-dired-explorer-hide ()
  "Hide the explorer window, keeping its buffer for next time."
  (interactive)
  (when-let* ((window (my-dired-explorer--window)))
    (delete-window window)))

;;;###autoload
(defun my-dired-explorer-toggle ()
  "Toggle the explorer."
  (interactive)
  (if (my-dired-explorer--window)
      (my-dired-explorer-hide)
    (my-dired-explorer-show)))

(defvar-local my-dired-explorer-directory
    '(:eval
      (let ((dir (file-name-nondirectory (directory-file-name default-directory)))
            (face (if (mode-line-window-selected-p)
                      'my-mode-line-emphasis
                    'mode-line-inactive)))
        (propertize dir
                    'help-echo (abbreviate-file-name default-directory)
                    'face face)))
  "Mode-line construct showing the explorer's current directory.")
(put 'my-dired-explorer-directory 'risky-local-variable t)

(defvar-keymap my-dired-explorer-mode-map
  :doc "Keymap for `my-dired-explorer-mode'."
  "RET"       #'my-dired-explorer-find-file
  "<mouse-1>" #'my-dired-explorer-find-file
  "^"         #'my-dired-explorer-up-directory
  "-"         #'my-dired-explorer-up-directory
  "q"         #'my-dired-explorer-hide)
(set-keymap-parent my-dired-explorer-mode-map dired-mode-map)

(define-minor-mode my-dired-explorer-mode
  "Minor mode for the dired explorer."
  :interactive nil
  :keymap my-dired-explorer-mode-map
  (when my-dired-explorer-mode
    (dired-hide-details-mode +1)
    (setq-local mode-line-format
                '(:eval (list my-mode-line-window-status-tag
                              " "
                              my-dired-explorer-directory)))))

;;; Auto-follow (opt-in hack) ------------------------------------------
;;
;; When `my-dired-explorer-follow-mode' is on, the explorer re-points
;; itself at the directory of whatever file you switch to and highlights
;; that file.
;;
;; Caveats worth understanding before relying on this:
;;
;;   * It hangs off `window-buffer-change-functions', which runs in a
;;     redisplay-adjacent context.  We mutate windows from inside it
;;     (kill/recreate the dired buffer, call `display-buffer'), which is
;;     exactly the kind of reentrancy that hook warns about -- hence the
;;     `my-dired-explorer--following' guard below.
;;
;;   * `my-dired-explorer--visit' kills and rebuilds the listing every
;;     time the directory changes.  We skip the rebuild when the target
;;     directory is unchanged (the common case: moving between files in
;;     one directory), which removes most of the flicker.  A production
;;     version would reuse one buffer and change its directory in place
;;     instead of kill/recreate.

(defvar my-dired-explorer--following nil
  "Reentrancy guard for `my-dired-explorer--follow'.")

(defun my-dired-explorer--follow (&rest _)
  "Point the explorer at the current buffer's file, if it is visible."
  (unless my-dired-explorer--following
    (when (and (my-dired-explorer--window)
               (buffer-file-name)
               (not (eq (current-buffer) my-dired-explorer--buffer)))
      (let ((my-dired-explorer--following t)
            (file (buffer-file-name)))
        (let ((dir (file-name-directory file)))
          ;; Only rebuild the listing when the directory actually changes.
          (unless (equal (file-truename dir)
                         (file-truename
                          (buffer-local-value 'default-directory
                                              my-dired-explorer--buffer)))
            (save-selected-window
              (my-dired-explorer--visit dir)))
          (when (buffer-live-p my-dired-explorer--buffer)
            (with-current-buffer my-dired-explorer--buffer
              (dired-goto-file file))))))))

(define-minor-mode my-dired-explorer-follow-mode
  "Make the explorer follow the file you are editing."
  :global t
  (if my-dired-explorer-follow-mode
      (add-hook 'window-buffer-change-functions #'my-dired-explorer--follow)
    (remove-hook 'window-buffer-change-functions #'my-dired-explorer--follow)))

;;; -------------------------------------------------------------------

(bind-keys :map my-ctl-z-map
           ("d" . my-dired-explorer-toggle))

(provide 'my-dired-explorer)
