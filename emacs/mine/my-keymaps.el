;;; my-keymaps.el -*- lexical-binding: t; -*-

;; my-jump-map
(define-prefix-command 'my-jump-map)
(keymap-global-set "C-c j" my-jump-map)

;; my-magit-map
(define-prefix-command 'my-magit-map)
(keymap-global-set "C-c g" my-magit-map)

;; my-org-map
(define-prefix-command 'my-org-map)
(keymap-global-set "C-c o" my-org-map)

;; my-toggle-map
(define-prefix-command 'my-toggle-map)
(keymap-global-set "C-c t" my-toggle-map)

(defvar my-cli-copy-command
  (cond ((eq system-type 'darwin) "pbcopy")
        ((eq system-type 'gnu/linux) "wl-copy")))

(defvar my-cli-paste-command
  (cond ((eq system-type 'darwin) "pbpaste")
        ((eq system-type 'gnu/linux) "wl-paste")))

(defun my-cut-to-clipboard (beg end &optional region)
  (interactive (progn
                 (let ((beg (mark))
                       (end (point)))
                   (unless (and beg end)
                     (user-error "The mark is not set now, so there is no region"))
                   (list beg end 'region))))
  (let ((cli-copy-command (split-string my-cli-copy-command)))
    (apply #'call-process-region `(,beg ,end ,(car cli-copy-command) t 0 nil ,@(cdr cli-copy-command)))))

(keymap-global-set "C-c C-w" #'my-cut-to-clipboard)

(defun my-copy-to-clipboard (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (let ((cli-copy-command (split-string my-cli-copy-command)))
    (apply #'call-process-region `(,beg ,end ,(car cli-copy-command) nil 0 nil ,@(cdr cli-copy-command)))))

(keymap-global-set "C-c M-w" #'my-copy-to-clipboard)

(defun my-paste-from-clipboard (&optional arg)
  (interactive "*P")
  (let ((cli-paste-command (split-string my-cli-paste-command)))
    (apply #'call-process `(,(car cli-paste-command) nil t nil ,@(cdr cli-paste-command))))

  (when (consp arg)
    (goto-char (prog1 (mark t)
                 (set-marker (mark-marker) (point) (current-buffer))))))

(keymap-global-set "C-c C-y" #'my-paste-from-clipboard)


(provide 'my-keymaps)
