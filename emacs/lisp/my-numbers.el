;;; my-numbers.el -*- lexical-binding: t; -*-

(require 'rect)
(require 'rx)

(defconst my-numbers--pattern-binary
  (rx (and ?0 (= 1 (in "bB"))
           (one-or-more (in "01"))))
  "Define the regular expression for binary numbers.")

(defconst my-numbers--pattern-octal
  (rx (and ?0 (one-or-more (in "0-7"))))
  "Define the regular expression for octal numbers.")

(defconst my-numbers--pattern-hex
  (rx (and ?0 (= 1 (in "xX")) (one-or-more xdigit)))
  "Define the regular expression for hexadecimal numbers.")

(defconst my-numbers--pattern-decimal
  (rx (and (zero-or-one (or ?- ?+))
           (one-or-more digit)))
  "Define the regular expression for decimal numbers.")

(defconst my-numbers--pattern-priority-list
  `(,my-numbers--pattern-decimal
    ,my-numbers--pattern-hex
    ,my-numbers--pattern-octal
    ,my-numbers--pattern-binary)
  "Define a list of patterns in the order of priority.")

(defun my-numbers--search-for-number (beg end)
  "Search for a number with BEG and END."
  ;; (seq-some
  ;;  (lambda (pattern)
  ;;    (re-search-forward pattern (line-end-position) t))
  ;;  my-numbers--pattern-priority-list))

  (goto-char beg)

  ;; support decimals for now
  (re-search-forward my-numbers--pattern-decimal end t))

(defun my-numbers--increment-at-point-with-cols (beg-col end-col count)
  "BEG-COL and END-COL are the start coumn and end column in a row of
the active rectangle.
Increment each number by COUNT."
  (let ((beg (+ (point) beg-col))
        (end (+ (point) end-col)))
    (my-numbers--increment-at-point beg end count)))

(defun my-numbers--increment-at-point (beg end count)
  "BEG and END are the start position and end position in a row of
the active rectangle.
Increment each number by COUNT."
  (my-numbers--search-for-number beg end)
  (skip-chars-backward "[0-9]+")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (+ count (string-to-number (match-string 0))))))

;;;###autoload
(defun my-numbers-increment-at-point (count)
  "Increment the number at point by COUNT."
  (interactive "p")
  (my-numbers--increment-at-point (point) (pos-eol) count))

;;;###autoload
(defun my-numbers-decrement-at-point (count)
  "Decrement the number at point by COUNT."
  (interactive "p")
  (my-numbers--increment-at-point (point) (pos-eol) (- count)))

;;;###autoload
(defun my-numbers-increment-at-point-sequentially (beg end count)
  "Increment the number at point from BEG to END by COUNT sequentially."
  (interactive "r\np")
  (apply-on-rectangle #'my-numbers--increment-at-point-with-cols beg end count))

;;;###autoload
(defun my-numbers-decrement-at-point-sequentially (beg end count)
  "Decrement the number at point from BEG to END by COUNT sequentially."
  (interactive "r\np")
  (apply-on-rectangle #'my-numbers--increment-at-point-with-cols beg end (- count)))

(bind-keys :map my-ctl-z-map
           ("C-a" . my-numbers-increment-at-point)
           ("C-x" . my-numbers-decrement-at-point)
           ("M-a" . my-numbers-increment-at-point-sequentially)
           ("M-x" . my-numbers-decrement-at-point-sequentially)
           :repeat-map my-numbers-repeat-map
           ("C-a" . my-numbers-increment-at-point)
           ("C-x" . my-numbers-decrement-at-point))

(provide 'my-numbers)
