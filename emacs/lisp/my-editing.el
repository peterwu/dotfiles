;;; my-editing.el -*- lexical-binding: t; -*-

;; Editing functionalities inspired by VIM key binds
;; C-c to lead normal edits
;; C-u C-c to allow pass number of operations
;; C-c SPC {textobj} : mark textobj
;; C-c d   {textobj} : delete textobj to black hole
;; C-c k   {textobj} : kill textobj to kill ring
;; C-c M-k {textobj} : kill textobj to system clipboard
;; C-c y   {textobj} : yank/copy textobj to kill ring
;; C-c M-y {textobj} : yank/copy textobj to system clipboard

;; For example:
;; C-c d w       : delete a word
;; C-u 3 C-c d w : delete 3 words
;; C-c y "       : yank text surrounded by a pair of double quotes
;; C-u 3 C-c y " : yank text surrounded by a pair of double quotes after being expanded 3 times
;;
;; Look at this example:
;; ( ( ( a b c _ ) ) )
;; , where _ indicates where the point is.
;;
;; "C-u 3 C-c y (" expands the selection to the text surrounded by the outer
;; most round brackets as if the "C-c y (" were performed 3 times.

;; Surround operations
;; C-c s s {textobj} {delimiter}
;;     : surround textobj with delimiter (e.g. "C-c s s w [" )
;; C-c s c {delimiter1} {delimiter2}
;;     : change surrounding delimiters (e.g. "C-c s c [ {" )
;; C-c s d {delimiter}
;;     : delete surrounding delimiters (e.g. "C-c s d [" )
;;
;; For surround commands (C-c s s), C-u has different uses depending on the
;; textobj it works on.
;;
;; If the textobj is a delimiter, then C-u suggests an outer surround.
;; e.g. "C-u C-c s s ( [" would result:
;; (abc_) ==> [(abc_)]
;; The same result can be achieved by using a closing delimiter if different
;; from its opening counterpart.
;; e.g. "C-c s s ( ]" would produce the same result.
;; Hence, the following keys yield the same result, which is an outer surround.
;; "C-u C-c s s ( ["
;; "C-u C-c s s ( ]"
;; "C-c s s ( ]"
;;
;; If the textobj is a thing, then C-u may carry a number, indicating how many
;; things are to be worked on.
;;
;; e.g. "C-u 2 C-c s s w [" would result in 2 words being surrounded with []
;;
;; "C-c s c" and "C-c s d" commands do not support C-u.

;;
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
;; s : sentence
;; p : paragraph
;; x : sexp
;; for line, simply repeat the last used key
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

(defconst my-editing--thing-list
  '(letter
    word
    line
    sentence
    paragraph
    sexp)
  "Define a list of things.")

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

(defmacro my-editing--textobj-delimiter (delimiter)
  "Define the textobj between DELIMITERs."
  (let* ((fn (intern (format "my-editing--textobj-%s" (symbol-name delimiter))))
         (docstring (format "Expand regions surrounded by %ss N times." (symbol-name delimiter)))
         (delimiters (alist-get delimiter my-editing--delimiter-alist))
         (open-delimiter (car delimiters))
         (close-delimiter (cadr delimiters)))
    ;; If opening and closing delimiters are the same, perform a simple search.
    (if (string= open-delimiter close-delimiter)
        `(defun ,fn (&optional n)
           ,docstring
           (interactive)
           (let* ((point (point))
                  (beg)
                  (end))
             (when (search-backward ,open-delimiter nil t n)
               (setq beg (point)))

             (goto-char point)

             (when (search-forward ,close-delimiter nil t n)
               (setq end (point)))

             ;; select the region
             (unless (equal beg end)
               (cons beg end))))

      ;; If opening and closing delimiters are different, perform a nestable search.
      `(defun ,fn (&optional n)
         ,docstring
         (interactive)
         (let* ((point (point))
                (beg)
                (end))

           (dotimes (_ n)
             (goto-char (my-editing--find-char-nestable ,open-delimiter ,close-delimiter -1))
             (setq beg (point))

             (goto-char (my-editing--find-char-nestable ,close-delimiter ,open-delimiter +1))
             (setq end (point)))

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
      (backward-to-word 1)
      (forward-to-word 1)
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
      (setq end (1+ (point))))
    (cons beg end)))

(defun my-editing--textobj-sentence (&optional n)
  "Return N sentences."
  (interactive)
  (let ((beg)
        (end))
    (save-excursion
      (backward-sentence)
      (setq beg (point))
      (forward-sentence n)
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
  (push-mark beg nil t)
  (goto-char end)
  (exchange-point-and-mark))

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
  (let* ((action (symbol-name action))
         (textobj (symbol-name textobj))
         (action-fn (intern (format "my-editing--%s" action)))
         (textobj-fn (intern (format "my-editing--textobj-%s" textobj)))
         (fn (intern (format "my-editing-%s-%s" action textobj)))
         (fn-docstring (format "%s N %ss."
                               (if (equal (upcase action) action)
                                   action
                                 (capitalize action))
                               textobj)))
    `(defun ,fn (&optional n)
       ,fn-docstring
       (interactive "p")
       (let* ((textobj (,textobj-fn n))
              (beg (car textobj))
              (end (cdr textobj)))
         (,action-fn beg end)))))

;; Generate all the action-textobj paired functions
(let ((actions my-editing--action-list)
      (textobjs (append
                 ;; delimiters
                 (mapcar #'car my-editing--delimiter-alist)

                 ;; other text objects
                 my-editing--thing-list)))

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
        (my-editing-action-sentence (intern (format "my-editing-%s-sentence" action)))
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
                ("s" . ,my-editing-action-sentence)
                ("p" . ,my-editing-action-paragraph)
                ("x" . ,my-editing-action-sexp)

                ("\'" . ,my-editing-action-single-quote)
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
        (KILL   . "M-k")
        (yank   . "y")
        (YANK   . "M-y")))

(provide 'my-editing)
