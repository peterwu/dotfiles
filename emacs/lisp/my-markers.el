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

(bind-keys :map my-mark-map
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

(provide 'my-markers)
