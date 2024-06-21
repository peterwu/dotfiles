;; system clipboard operations: cut/copy/paste
(defun my-cut-to-clipboard (beg end)
  "Cut to the clipboard from BEG to END."
  (interactive "r")
  (if (display-graphic-p)
      (clipboard-kill-region beg end)
    (gui-set-selection 'CLIPBOARD
                       (buffer-substring-no-properties beg end))))

(defun my-copy-to-clipboard (beg end)
  "Copy to the clipboard from BEG to END."
  (interactive "r")
  (if (display-graphic-p)
      (clipboard-kill-ring-save beg end)
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
