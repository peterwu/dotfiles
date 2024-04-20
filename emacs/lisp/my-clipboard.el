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
  (interactive (progn
                 (let ((beg (mark))
                       (end (point)))
                   (unless (and beg end)
                     (user-error "The mark is not set now, so there is no region"))
                   (list beg end 'region))))
  (let ((cli-copy-command (split-string my-cli-copy-command)))
    (apply #'call-process-region `(,beg ,end ,(car cli-copy-command) t 0 nil ,@(cdr cli-copy-command)))))

(defun my-copy-to-clipboard (beg end &optional region)
  "Copy to the clipboard the REGION from BEG to END."
  (interactive (list (mark) (point) 'region))
  (let ((cli-copy-command (split-string my-cli-copy-command)))
    (apply #'call-process-region `(,beg ,end ,(car cli-copy-command) nil 0 nil ,@(cdr cli-copy-command)))))

(defun my-paste-from-clipboard (&optional arg)
  "Paste from the clipboard.
If ARG is omitted, point will not change; otherwise, point will move to the beginning of the pasted text."
  (interactive "*P")
  (let ((cli-paste-command (split-string my-cli-paste-command)))
    (apply #'call-process `(,(car cli-paste-command) nil t nil ,@(cdr cli-paste-command))))

  (when (consp arg)
    (goto-char (prog1 (mark t)
                 (set-marker (mark-marker) (point) (current-buffer))))))

(bind-keys
 ("C-c C-w" . my-cut-to-clipboard)
 ("C-c M-w" . my-copy-to-clipboard)
 ("C-c C-y" . my-paste-from-clipboard))

(provide 'my-clipboard)
