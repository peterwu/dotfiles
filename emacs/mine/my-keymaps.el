;;; my-keymaps.el -*- lexical-binding: t; -*-

;; my-jump-map
(bind-keys :prefix-map my-jump-map
           :prefix "C-c j")

;; my-magit-map
(bind-keys :prefix-map my-magit-map
           :prefix "C-c g")

;; my-org-map
(bind-keys :prefix-map my-org-map
           :prefix "C-c o")

;; my-toggle-map
(bind-keys :prefix-map my-toggle-map
           :prefix "C-c t")

;; system clipboard operations: cut/copy/paste
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

(defun my-copy-to-clipboard (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (let ((cli-copy-command (split-string my-cli-copy-command)))
    (apply #'call-process-region `(,beg ,end ,(car cli-copy-command) nil 0 nil ,@(cdr cli-copy-command)))))

(defun my-paste-from-clipboard (&optional arg)
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

;; my simulation of vim visual commands

(defun my-mark-line ()
  (interactive)
  (beginning-of-visual-line)
  (push-mark nil nil t)
  (end-of-visual-line))

(defun my-mark-letter ()
  (interactive)
  (push-mark nil nil t)
  (forward-char))

(defun my-mark-word ()
  (interactive)
  (backward-word)
  (push-mark nil nil t)
  (forward-word))

(defun my-mark-sentence ()
  (interactive)
  (backward-sentence)
  (push-mark nil nil t)
  (forward-sentence))

(defun my-mark-paragraph ()
  (interactive)
  (backward-paragraph)
  (push-mark nil nil t)
  (forward-paragraph))

(defun my-mark-buffer ()
  (interactive)
  (beginning-of-buffer)
  (push-mark nil nil t)
  (end-of-buffer))

(defun my-mark-sexp ()
  (interactive)
  (backward-sexp)
  (push-mark nil nil t)
  (forward-sexp))

(defun my-find-distance (p1 p2)
  "Return the absolute distance between the positions P1 to P2."
  (let* ((a1 (abs p1))
         (a2 (abs (or p2 most-positive-fixnum))))
    (- (max a1 a2) (min a1 a2))))

;; https://github.com/mkleehammer/surround/blob/main/surround.el
(defun my-find-char-nestable (char other dir)
  (if (looking-at (regexp-quote char))
      (when (= dir 1)
        (forward-char 1))

    (if (and (looking-at (regexp-quote other)) (= dir 1))
        (forward-char dir))

    (let ((level 1))                      ; level of nesting
      (while (> level 0)
        (let* ((current (point))
               (charpos (search-forward char nil t dir))
               (otherpos (progn
                           (goto-char current)
                           (search-forward other nil t dir)))
               (chardist (my-find-distance current charpos))
               (otherdist (my-find-distance current otherpos))
               (pos (if (< chardist otherdist) charpos otherpos))
               (diff (if (< chardist otherdist) -1 1)))

          (if (null charpos)
              (user-error "Did not find %s" char))

          (setq level (+ level diff))
          (goto-char pos)))))

  (point))

(defvar my-delimiter-alist
  '((angle-bracket  . ("<" ">"))
    (curly-bracket  . ("{" "}"))
    (round-bracket  . ("(" ")"))
    (square-bracket . ("[" "]"))

    (single-quote . ("\'" "\'"))
    (double-quote . ("\"" "\""))))

(defmacro my-mark-delimiter (delimiter)
  (let ((mark-fn (intern (concat "my-mark-" (symbol-name delimiter)))))
    `(defun ,mark-fn (&optional arg)
       (interactive "*P")

       (let* ((delimiters (alist-get ',delimiter my-delimiter-alist))
              (open-delimiter (nth 0 delimiters))
              (close-delimiter (nth 1 delimiters))
              (p0 (point))
              (p1 (point))
              (p2 (point)))

         (if (string= open-delimiter close-delimiter)
             (progn
               (when (search-backward open-delimiter nil t 1)
                 (unless arg (forward-char))
                 (setq p1 (point)))

               (goto-char p0)

               (when (search-forward close-delimiter nil t 1)
                 (unless arg (backward-char))
                 (setq p2 (point))))

           (progn
             (goto-char (my-find-char-nestable open-delimiter close-delimiter -1))
             (unless arg (forward-char))
             (setq p1 (point))

             (goto-char (my-find-char-nestable close-delimiter open-delimiter +1))
             (unless arg (backward-char))

             (setq p2 (point))))

         ;; select the region
         (unless (equal p1 p2)
           (goto-char p1)
           (push-mark nil nil t)
           (goto-char p2))))))

(mapc (lambda (delimiter)
        (eval `(my-mark-delimiter ,delimiter)))
      (mapcar 'car my-delimiter-alist))

(bind-keys :prefix-map my-mark-object-map
           :prefix "C-c SPC"
           ("SPC" . my-mark-line)
           ("l"   . my-mark-letter)
           ("w"   . my-mark-word)
           ("s"   . my-mark-sentence)
           ("p"   . my-mark-paragraph)
           ("x"   . my-mark-sexp)
           ("b"   . my-mark-buffer)

           ("<" . my-mark-angle-bracket)
           (">" . my-mark-angle-bracket)
           ("{" . my-mark-curly-bracket)
           ("}" . my-mark-curly-bracket)
           ("(" . my-mark-round-bracket)
           (")" . my-mark-round-bracket)
           ("[" . my-mark-square-bracket)
           ("]" . my-mark-square-bracket)

           ("\'" . my-mark-single-quote)
           ("\"" . my-mark-double-quote))

(provide 'my-keymaps)
