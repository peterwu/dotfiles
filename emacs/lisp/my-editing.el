;;; my-editing.el -*- lexical-binding: t; -*-

;; Editing functionalities inspired by VIM key binds
;; C-z to lead normal edits
;; C-u C-z to allow pass applicable argument
;; C-z SPC {textobj} : mark textobj
;; C-z d   {textobj} : delete textobj to black hole
;; C-z k   {textobj} : kill textobj to kill ring
;; C-z K   {textobj} : kill textobj to system clipboard
;; C-z y   {textobj} : yank/copy textobj to kill ring
;; C-z Y   {textobj} : yank/copy textobj to system clipboard

;; Surround operations
;; C-z s {textobj} {delimiter}
;;     : surround textobj with delimiter (e.g. "C-z s w [" )
;; C-z s c {delimiter1} {delimiter2}
;;     : change surrounding delimiters (e.g. "C-z s c [ {" )
;; C-z s d {delimiter}
;;     : delete surrounding delimiters (e.g. "C-z s d [" )

;; For example:
;; C-z d w         : delete a word
;; C-u 3 C-z d w   : delete 3 words
;; C-u C-z d w     : delete 4 words
;; C-u C-u C-z d w : delete 16 words as universal argument works
;; C-z y "         : yank text within surrounding ""
;; C-u C-z y "     : yank text around surrounding ""
;; C-z s c ' "     : change surrounding ' to "
;; C-z s d [       : delete surrounding []
;; C-z s w [       : surround word with []
;; C-u 3 s w [     : surround 3 words with []
;; C-z s ) }       : surround inner text surrounded by () with {}
;; C-u C-z s ) }   : surround outer text surrounded by () with {}


;; Text Objects include Delimiters and Things
;;
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

;; Things:
;; l : letter
;; w : word
;; p : paragraph
;; x : sexp
;; for line, simply repeat the last used key
;; e.g.
;; C-z SPC SPC : mark the line
;; C-z d d     : delete the line
;; C-z y y     : yank the line to kill ring
;; C-z Y Y     : yank the line to system clipboard

(defconst my-editing--delimiter-alist
  '((angle-bracket  . ("<" ">"))
    (curly-bracket  . ("{" "}"))
    (round-bracket  . ("(" ")"))
    (square-bracket . ("[" "]"))

    (single-quote . ("'" "'"))
    (double-quote . ("\"" "\"")))
  "Define a list of opening and closing delimiters.")

(defconst my-editing--thing-list
  '(letter
    word
    line
    paragraph
    sexp)
  "Define a list of things.")

(defconst my-editing--textobj-list
  (append
   ;; delimiters
   (mapcar #'car my-editing--delimiter-alist)

   ;; things
   my-editing--thing-list)
  "Define a list of text objects.")

(defconst my-editing--action-list
  '(mark
    delete
    kill
    KILL
    yank
    YANK)
  "Define a list of actions.")

(defun my-editing--find-distance (p1 p2)
  "Return the absolute distance between the positions P1 to P2."
  (let* ((a1 (abs p1))
         (a2 (abs (or p2 most-positive-fixnum))))
    (- (max a1 a2) (min a1 a2))))

;; https://github.com/mkleehammer/surround/blob/main/surround.el
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

(defun my-editing--get-delimiter-cons (delimiter)
  "Return the textobj symbol name of DELIMITER."
  (seq-some
   (lambda (cons)
     (let ((pair (cdr cons)))
       (when (member delimiter pair)
         cons)))
   my-editing--delimiter-alist))

(defmacro my-editing--textobj-delimiter (delimiter)
  "Define the textobj between DELIMITERs."
  (let* ((fn (intern (format "my-editing--textobj-%s" (symbol-name delimiter))))
         (docstring (format "Expand regions surrounded by %ss N times." (symbol-name delimiter)))
         (delimiters (alist-get delimiter my-editing--delimiter-alist))
         (open-delimiter (car delimiters))
         (close-delimiter (cadr delimiters)))
    ;; If opening and closing delimiters are the same, perform a simple search.
    (if (string= open-delimiter close-delimiter)
        `(defun ,fn (&optional arg)
           ,docstring
           (interactive)
           (let* ((point (point))
                  (beg)
                  (end))
             (when (search-backward ,open-delimiter nil t 1)
               (setq beg (point)))

             (goto-char point)

             (when (search-forward ,close-delimiter nil t 1)
               (setq end (point)))

             ;; select the region
             (unless (equal beg end)
               (cons beg end))))

      ;; If opening and closing delimiters are different, perform a nestable search.
      `(defun ,fn (&optional arg)
         ,docstring
         (interactive)
         (let* ((point (point))
                (beg)
                (end))

           (goto-char (my-editing--find-char-nestable ,open-delimiter ,close-delimiter -1))
           (unless arg (forward-char))
           (setq beg (point))

           (goto-char (my-editing--find-char-nestable ,close-delimiter ,open-delimiter +1))
           (unless arg (backward-char))
           (setq end (point))

           ;; select the region
           (unless (equal beg end)
             (cons beg end)))))))

;; Generate all the delimiter text objects
(mapc (lambda (delimiter)
        (eval
         `(my-editing--textobj-delimiter ,delimiter)))
      (mapcar #'car my-editing--delimiter-alist))

;; thing text objects
(defun my-editing--textobj-letter (&optional n)
  "Return N letters."
  (interactive)
  (let ((beg)
        (end))
    (save-excursion
      (setq beg (point))
      (forward-char n)
      (setq end (point)))
    (cons beg end)))

(defun my-editing--textobj-word (&optional n)
  "Return N words."
  (interactive)
  (let ((beg)
        (end))
    (save-excursion
      (forward-char)
      (backward-word)
      (setq beg (point))
      (forward-word n)
      (setq end (point)))
    (cons beg end)))

(defun my-editing--textobj-line (&optional n)
  "Return N visual lines."
  (interactive)
  (let ((beg)
        (end))
    (save-excursion
      (beginning-of-visual-line)
      (setq beg (point))
      (end-of-visual-line n)
      (setq end (point)))
    (cons beg end)))

(defun my-editing--textobj-paragraph (&optional n)
  "Return N paragraphs."
  (interactive)
  (let ((beg)
        (end))
    (save-excursion
      (backward-paragraph)
      (setq beg (point))
      (forward-paragraph n)
      (setq end (point)))
    (cons beg end)))

(defun my-editing--textobj-sexp (&optional n)
  "Return N sexps."
  (interactive)
  (let ((beg)
        (end))
    (save-excursion
      (backward-sexp)
      (setq beg (point))
      (forward-sexp n)
      (setq end (point)))
    (cons beg end)))

;; actions
(defun my-editing--mark (beg end)
  (push-mark end nil t)
  (goto-char beg))

(defun my-editing--delete (beg end)
  (delete-region beg end))

(defun my-editing--kill (beg end)
  (kill-region beg end))

(defun my-editing--KILL (beg end)
  (my-copy-to-clipboard beg end)
  (delete-region beg end))

(defun my-editing--yank (beg end)
  (kill-ring-save beg end))

(defun my-editing--YANK (beg end)
  (my-copy-to-clipboard beg end))

(defmacro my-editing-action-textobj (action textobj)
  "Generate my-editing-ACTION-TEXTOBJ commands."
  (let* ((code (if (member textobj my-editing--thing-list)
                   "p"
                 "P"))
         (action (symbol-name action))
         (textobj (symbol-name textobj))
         (action-fn (intern (format "my-editing--%s" action)))
         (textobj-fn (intern (format "my-editing--textobj-%s" textobj)))
         (fn (intern (format "my-editing-%s-%s" action textobj)))
         (fn-docstring (format "%s N %ss."
                               (if (equal (upcase action) action)
                                   action
                                 (capitalize action))
                               textobj)))
    `(defun ,fn (&optional arg)
       ,fn-docstring
       (interactive ,code)
       (let* ((textobj (,textobj-fn arg))
              (beg (car textobj))
              (end (cdr textobj)))
         (,action-fn beg end)))))

;; Generate all the action-textobj paired functions
(let ((actions my-editing--action-list)
      (textobjs my-editing--textobj-list))
  (mapc (lambda (action)
          (mapc (lambda (textobj)
                  (eval
                   `(my-editing-action-textobj ,action ,textobj)))
                textobjs))
        actions))

;; Create a macro to ease key binds
(defmacro my-editing--bind-keys (action line-key)
  (let ((map (intern (format "my-%s-map" (symbol-name action))))

        ;; my-editing-action-textobj
        (my-editing-action-line (intern (format "my-editing-%s-line" action)))
        (my-editing-action-letter (intern (format "my-editing-%s-letter" action)))
        (my-editing-action-word (intern (format "my-editing-%s-word" action)))
        (my-editing-action-paragraph (intern (format "my-editing-%s-paragraph" action)))
        (my-editing-action-sexp (intern (format "my-editing-%s-sexp" action)))

        (my-editing-action-single-quote (intern (format "my-editing-%s-single-quote" action)))
        (my-editing-action-double-quote (intern (format "my-editing-%s-double-quote" action)))
        (my-editing-action-angle-bracket (intern (format "my-editing-%s-angle-bracket" action)))
        (my-editing-action-curly-bracket (intern (format "my-editing-%s-curly-bracket" action)))
        (my-editing-action-round-bracket (intern (format "my-editing-%s-round-bracket" action)))
        (my-editing-action-square-bracket (intern (format "my-editing-%s-square-bracket" action))))

    `(bind-keys :map ,map
                (,line-key . ,my-editing-action-line)

                ("l" . ,my-editing-action-letter)
                ("w" . ,my-editing-action-word)
                ("p" . ,my-editing-action-paragraph)
                ("x" . ,my-editing-action-sexp)

                ("'"  . ,my-editing-action-single-quote)
                ("\"" . ,my-editing-action-double-quote)
                ("<"  . ,my-editing-action-angle-bracket)
                (">"  . ,my-editing-action-angle-bracket)
                ("{"  . ,my-editing-action-curly-bracket)
                ("}"  . ,my-editing-action-curly-bracket)
                ("("  . ,my-editing-action-round-bracket)
                (")"  . ,my-editing-action-round-bracket)
                ("["  . ,my-editing-action-square-bracket)
                ("]"  . ,my-editing-action-square-bracket))))

;; Bind keys to action and its line-key performed on the line textobj
(mapc (lambda (cons)
        (let ((action (car cons))
              (line-key (cdr cons)))
          (eval
           `(my-editing--bind-keys ,action ,line-key))))
      '((mark   . "SPC")
        (delete . "d")
        (kill   . "k")
        (KILL   . "K")
        (yank   . "y")
        (YANK   . "Y")))

;; surround
(defun my-editing-surround-change (delimiter1 delimiter2)
  "Change surrounding DELIMITER1 to DELIMITER2."
  (interactive "cChange surround from:\ncChange surround to:")
  (save-excursion
    (let* ((cons1 (my-editing--get-delimiter-cons (string delimiter1)))
           (textobj1 (symbol-name (car cons1)))

           (cons2 (my-editing--get-delimiter-cons (string delimiter2)))
           (pair2 (cdr cons2))
           (opening2 (car pair2))
           (closing2 (cadr pair2))

           (textobj1-fn (intern (format "my-editing--textobj-%s" textobj1)))
           (bounds (funcall textobj1-fn 1))
           (beg (car bounds))
           (end (cdr bounds)))

      (goto-char end)
      (delete-char -1)
      (insert closing2)
      (goto-char beg)
      (delete-char 1)
      (insert opening2))))

(defun my-editing-surround-delete (delimiter)
  "Delete surrounding DELIMITERs."
  (interactive "cDelete surround:")
  (save-excursion
    (let* ((cons (my-editing--get-delimiter-cons (string delimiter)))
           (textobj (symbol-name (car cons)))
           (textobj-fn (intern (format "my-editing--textobj-%s" textobj)))
           (bounds (funcall textobj-fn 1))
           (beg (car bounds))
           (end (cdr bounds)))

      (goto-char end)
      (delete-char -1)

      (goto-char beg)
      (delete-char 1))))

(defmacro my-editing--surround (textobj)
  "Surround textobj."
  (let ((code (if (member textobj my-editing--thing-list)
                  "p"
                "P"))
        (textobj (symbol-name textobj))
        (textobj-fn (intern (format "my-editing--textobj-%s" textobj)))
        (fn (intern (format "my-editing-surround-%s" textobj)))
        (docstring (format "Surround %s with delimiters." textobj)))
    `(defun ,fn (delimiter &optional arg)
       ,docstring
       (interactive ,(format "cSurround with:\n%s" code))
       (save-excursion
         (let* ((cons (my-editing--get-delimiter-cons (string delimiter)))
                (pair (cdr cons))
                (opening (car pair))
                (closing (cadr pair))

                (textobj-fn (intern (format "my-editing--textobj-%s" ,textobj)))
                (bounds (funcall textobj-fn arg))
                (beg (car bounds))
                (end (cdr bounds)))

           (goto-char end)
           (insert closing)
           (goto-char beg)
           (insert opening))))))

(mapc (lambda (textobj)
        (eval
         `(my-editing--surround ,textobj)))
      my-editing--textobj-list)

(bind-keys :map my-surround-map
           ("c" . my-editing-surround-change)
           ("d" . my-editing-surround-delete)

           ("s" . my-editing-surround-line)
           ("l" . my-editing-surround-letter)
           ("w" . my-editing-surround-word)
           ("p" . my-editing-surround-paragraph)
           ("x" . my-editing-surround-sexp)

           ("'"  . my-editing-surround-single-quote)
           ("\"" . my-editing-surround-double-quote)
           ("<"  . my-editing-surround-angle-bracket)
           (">"  . my-editing-surround-angle-bracket)
           ("{"  . my-editing-surround-curly-bracket)
           ("}"  . my-editing-surround-curly-bracket)
           ("("  . my-editing-surround-round-bracket)
           (")"  . my-editing-surround-round-bracket)
           ("["  . my-editing-surround-square-bracket)
           ("]"  . my-editing-surround-square-bracket))

(provide 'my-editing)
