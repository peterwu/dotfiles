;;; my-editing.el -*- lexical-binding: t; -*-

;; Editing functionalities inspired by VIM key binds
;; C-c to lead normal edits
;; C-u C-c to allow pass number of operations

;; C-c d   : delete textobj to black hole
;; C-c k   : kill textobj to kill ring
;; C-c M-k : kill textobj to system clipboard
;; C-c SPC : mark textobj
;; C-c y   : yank/copy textobj to kill ring
;; C-c M-y : yank/copy textobj to system clipboard
;; C-c p   : paste after point
;; C-c P   : paste before point
;; C-c M-p : paste after point from system clipboard
;; C-c M-P : paste before point from system clipboard

;; For example:
;; C-c d w : delete a word
;; C-u 3 C-c d w : delete 3 words
;; C-c y " : yank text surrounded by a pair of double quotes
;; C-u 3 C-c y " : yank text surrounded by the region after expanded 3 times
;;
;; Look at this example:
;; ( ( ( a b c _ ) ) )
;; , where _ indicates where the point is.
;;
;; "C-u 3 C-c y (" expands the selection to the text surrounded by the outer
;; most round brackets as if the "C-c y (" were performed 3 times.

;; Text Objects
;; Delimiters:
;; ' : single-quote
;; " : double-quote
;; < : angle-bracket
;; > : angle-bracket
;; { : curly-bracket
;; } : curly-bracket
;; ( : round-bracket
;; ) : round-bracket
;; [ : square-bracket
;; ] : square-bracket

;; Others:
;; l : letter
;; w : word
;; s : sentence
;; p : paragraph
;; x : sexp
;; for line, simply repeat the last used letter
;; e.g.
;; C-c SPC SPC : mark the line
;; C-c d d     : delete the line
;; C-c y y     : yank the line to kill ring
;; C-c M-y M-y : yank the line to system clipboard

(defconst my-editing--delimiter-alist
  '((angle-bracket  . ("<" ">"))
    (curly-bracket  . ("{" "}"))
    (round-bracket  . ("(" ")"))
    (square-bracket . ("[" "]"))

    (single-quote . ("\'" "\'"))
    (double-quote . ("\"" "\"")))
  "Define a list of opening and closing delimiters.")

(defun my-editing--find-distance (p1 p2)
  "Return the absolute distance between the positions P1 to P2."
  (let* ((a1 (abs p1))
         (a2 (abs (or p2 most-positive-fixnum))))
    (- (max a1 a2) (min a1 a2))))

(defun my-editing--find-char-nestable (char other dir)
  "Find the matching CHAR and OTHER.
If DIR is 1, search forward; if DIR is -1, search backward."
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
               (chardist (my-editing--find-distance current charpos))
               (otherdist (my-editing--find-distance current otherpos))
               (pos (if (< chardist otherdist) charpos otherpos))
               (diff (if (< chardist otherdist) -1 1)))

          (if (null charpos)
              (user-error "Did not find %s" char))

          (setq level (+ level diff))
          (goto-char pos)))))

  (point))

(defmacro my-editing--textobj-delimiter (delimiter)
  "Define the textobj between DELIMITERs."
  (let ((editing-fn (intern (format "my-editing--textobj-%s" (symbol-name delimiter)))))
    `(defun ,editing-fn (&optional n)
       ,(format "Expand regions surrounded by %ss N times." (symbol-name delimiter))
       (interactive)

       (let* ((delimiters ',(alist-get delimiter my-editing--delimiter-alist))
              (open-delimiter (nth 0 delimiters))
              (close-delimiter (nth 1 delimiters))
              (point (point))
              (start)
              (end))

         ;; If opening and closing delimiters are the same, perform a simple search.
         (if (string= open-delimiter close-delimiter)
             (progn
               (when (search-backward open-delimiter nil t n)
                 (setq start (point)))

               (goto-char point)

               (when (search-forward close-delimiter nil t n)
                 (setq end (point))))

           ;; If opening and closing delimiters are different, perform a nestable search.
           (progn
             (dotimes (_ n)
               (goto-char (my-editing--find-char-nestable open-delimiter close-delimiter -1))
               (setq start (point))

               (goto-char (my-editing--find-char-nestable close-delimiter open-delimiter +1))
               (setq end (point)))))

         ;; select the region
         (unless (equal start end)
           (cons start end))))))

;; Generate all the delimiter text objects
(mapc (lambda (delimiter)
        (eval
         `(my-editing--textobj-delimiter ,delimiter)))
      (mapcar 'car my-editing--delimiter-alist))

(defun my-editing--textobj-letter (&optional n)
  "Return N letters."
  (interactive)
  (let ((start)
        (end))
    (save-excursion
      (setq start (point))
      (forward-char n)
      (setq end (point)))
    (cons start end)))

(defun my-editing--textobj-word (&optional n)
  "Return N words."
  (interactive)
  (let ((start)
        (end))
    (save-excursion
      (backward-to-word 1)
      (forward-to-word 1)
      (setq start (point))
      (forward-word n)
      (setq end (point)))
    (cons start end)))

(defun my-editing--textobj-line (&optional n)
  "Return N visual lines."
  (interactive)
  (let ((start)
        (end))
    (save-excursion
      (beginning-of-visual-line)
      (setq start (point))
      (end-of-visual-line n)
      (setq end (point)))
    (cons start end)))

(defun my-editing--textobj-sentence (&optional n)
  "Return N sentences."
  (interactive)
  (let ((start)
        (end))
    (save-excursion
      (backward-sentence)
      (setq start (point))
      (forward-sentence n)
      (setq end (point)))
    (cons start end)))

(defun my-editing--textobj-paragraph (&optional n)
  "Return N paragraphs."
  (interactive)
  (let ((start)
        (end))
    (save-excursion
      (backward-paragraph)
      (setq start (point))
      (forward-paragraph n)
      (setq end (point)))
    (cons start end)))

(defun my-editing--textobj-sexp (&optional n)
  "Return N sexps."
  (interactive)
  (let ((start)
        (end))
    (save-excursion
      (backward-sexp)
      (setq start (point))
      (forward-sexp n)
      (setq end (point)))
    (cons start end)))

;; action : mark, delete, kill, yank
;; textobj : letter, word, line, sentence, paragraph, sexp
(defmacro my-editing-action-textobj (action textobj)
  (let ((edit-fn (intern (format "my-editing-%s-%s" (symbol-name action) (symbol-name textobj))))
        (textobj-fn (intern (format "my-editing--textobj-%s" (symbol-name textobj)))))
    `(defun ,edit-fn (&optional n)
       (interactive "p")
       (let* ((textobj (,textobj-fn n))
              (start (car textobj))
              (end (cdr textobj)))
         (cond
          ((eq ',action 'mark)
           (goto-char start)
           (push-mark nil nil t)
           (goto-char end))

          ((eq ',action 'delete)
           (delete-region start end))

          ((eq ',action 'kill)
           (kill-region start end))

          ((eq ',action 'yank)
           (kill-ring-save start end)))))))

;; Generate all the action-textobj paired functions
(let ((actions '(mark
                 delete
                 kill
                 yank))
      (textobjs (append
                 ;; delimiters
                 (mapcar 'car my-editing--delimiter-alist)

                 ;; other text objects
                 '(letter
                   word
                   line
                   sentence
                   paragraph
                   sexp))))
  (mapc (lambda (action)
          (mapc (lambda (textobj)
                  (eval
                   `(my-editing-action-textobj ,action ,textobj)))
                textobjs))
        actions))

;; key binds


(provide 'my-editing)
