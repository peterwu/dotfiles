;; system clipboard operations: cut/copy/paste
(defvar my-cli-copy-command
  (cond ((eq system-type 'darwin) "pbcopy")
        ((eq system-type 'gnu/linux) "wl-copy"))
  "Define the command line utility to perform copy operation.")

(defvar my-cli-paste-command
  (cond ((eq system-type 'darwin) "pbpaste")
        ((eq system-type 'gnu/linux) "wl-paste"))
  "Define the command line utility to perform paste operation.")

(defun my-cut-to-clipboard (beg end &optional region)
  "Cut to the clipboard the REGION from BEG to END."
  (interactive (let ((beg (mark))
                     (end (point)))
                 (unless (and beg end)
                   (user-error "The mark is not set now, so there is no region"))
                 (list beg end 'region)))
  (if (display-graphic-p)
      (clipboard-kill-region beg end region)
    (gui-set-selection 'CLIPBOARD
                       (buffer-substring-no-properties beg end))))

(defun my-copy-to-clipboard (beg end &optional region)
  "Copy to the clipboard the REGION from BEG to END."
  (interactive (list (mark) (point) 'region))
  (if (display-graphic-p)
      (clipboard-kill-ring-save beg end region)
    (gui-set-selection 'CLIPBOARD
                       (buffer-substring-no-properties beg end))))

(defun my-paste-from-clipboard (&optional arg)
  "Paste from the clipboard.
If ARG is omitted, point will not change; otherwise, point will move to the beginning of the pasted text."
  (interactive "*P")
  (if (display-graphic-p)
      (clipboard-yank)
    (insert (gui-get-selection 'CLIPBOARD 'STRING)))

  (when (consp arg)
    (goto-char (prog1 (mark t)
                 (set-marker (mark-marker) (point) (current-buffer))))))

(bind-keys :map my-ctl-z-map
           ("C-w" . my-cut-to-clipboard)
           ("M-w" . my-copy-to-clipboard)
           ("C-y" . my-paste-from-clipboard))

(provide 'my-clipboard)
