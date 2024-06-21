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

(defun my-numbers--search-for-number ()
  "Search for a number."
  ;; (seq-some
  ;;  (lambda (pattern)
  ;;    (re-search-forward pattern (line-end-position) t))
  ;;  my-numbers--pattern-priority-list))

  ;; support decimals for now
  (re-search-forward my-numbers--pattern-decimal (line-end-position) t))

;;;###autoload
(defun my-numbers-increment-at-point (&optional n)
  "Increment the number at point by N."
  (interactive "p")
  (my-numbers--search-for-number)
  (skip-chars-backward "[0-9]+")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (+ n (string-to-number (match-string 0))))))

;;;###autoload
(defun my-numbers-decrement-at-point (&optional n)
  "Decrement the number at point by N."
  (interactive "p")
  (my-numbers-increment-at-point (* -1 n)))

(bind-keys :map my-ctl-z-map
           ("C-a" . my-numbers-increment-at-point)
           ("C-x" . my-numbers-decrement-at-point)
           :repeat-map my-ctl-z-repeat-map
           ("C-a" . my-numbers-increment-at-point)
           ("C-x" . my-numbers-decrement-at-point))

(provide 'my-numbers)
