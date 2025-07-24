;;; my-surround.el -*- lexical-binding: t; -*-

(defconst my-surround--pairs-alist
  '((?\( . ("(" . ")"))
    (?\[ . ("[" . "]"))
    (?\{ . ("{" . "}"))

    (?\` . ("`"  . "`"))
    (?\' . ("'"  . "'"))
    (?\" . ("\"" . "\""))

    (?\) . ("(" . ")"))
    (?\] . ("[" . "]"))
    (?\} . ("{" . "}"))

    (?b . ("(" . ")"))
    (?B . ("{" . "}"))
    (?< . ("<" . ">"))
    (?> . ("<" . ">"))))

(defun my-surround--find-distance (p1 p2)
  "Return the absolute distance between the positions P1 to P2."
  (let* ((a1 (abs p1))
         (a2 (abs (or p2 most-positive-fixnum))))
    (- (max a1 a2) (min a1 a2))))

(defun my-surround--find-char-nestable (char other dir)
  "Return the position of the closest, unnested CHAR.

If DIR is +1, search forward; if DIR is -1, search backward."
  (save-excursion
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
                 (chardist (my-surround--find-distance current charpos))
                 (otherdist (my-surround--find-distance current otherpos))
                 (pos (if (< chardist otherdist) charpos otherpos))
                 (diff (if (< chardist otherdist) -1 1)))

            (if (null charpos)
                (user-error "Did not find %s" char))

            (setq level (+ level diff))
            (goto-char pos)))))

    (point)))

;;;###autoload
(defun my-surround-change (old-char new-char)
  (interactive "c\nc")

  (when-let* ((old-delims (assoc old-char my-surround--pairs-alist))
              (old-open-delim (cadr old-delims))
              (old-close-delim (cddr old-delims))

              (new-delims (assoc new-char my-surround--pairs-alist))
              (new-open-delim (cadr new-delims))
              (new-close-delim (cddr new-delims))

              (beg (my-surround--find-char-nestable
                    old-open-delim old-close-delim -1))
              (end (my-surround--find-char-nestable
                    old-close-delim old-open-delim +1)))
    (save-excursion
      (goto-char end)
      (delete-char -1)
      (insert new-close-delim)

      (goto-char beg)
      (delete-char +1)
      (insert new-open-delim))))

;;;###autoload
(defun my-surround-delete (char)
  (interactive "c")

  (when-let* ((delims (assoc char my-surround--pairs-alist))
              (open-delim (cadr delims))
              (close-delim (cddr delims))
              (beg (my-surround--find-char-nestable open-delim close-delim -1))
              (end (my-surround--find-char-nestable close-delim open-delim +1)))
    (save-excursion
      (goto-char end)
      (delete-char -1)

      (goto-char beg)
      (delete-char +1))))

;;;###autoload
(defun my-surround-region (beg end char)
  (interactive "r\nc")

  (when-let* ((delims (assoc char my-surround--pairs-alist))
              (open-delim (cadr delims))
              (close-delim (cddr delims)))
    (save-excursion
      (goto-char end)
      (insert close-delim)

      (goto-char beg)
      (insert open-delim))))

(provide 'my-surround)
