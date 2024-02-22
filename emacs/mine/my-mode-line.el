;;; my-mode-line.el -*- lexical-binding: t; -*-

;; mode-line
(defun my-ellipsize-file-name (file-name max-length)
  (let* ((ellipsis (if (char-displayable-p ?…) "…" "..."))
         (left (/ max-length 2))
         (center (length ellipsis))
         (right (- max-length left center)))
    (if (> (length file-name) max-length)
        (concat
         (substring file-name 0 (1- left))
         " "
         ellipsis
         " "
         (substring file-name (- (length file-name) (1- right))))
      file-name)))

(defun my-mode-line-render (left centre right)
  "Return a string of `window-total-width' length.
    Containing LEFT, CENTRE and RIGHT aligned respectively."
  (let* ((left-width (string-width (format-mode-line left)))
         (centre-width (string-width (format-mode-line centre)))
         (right-width (string-width (format-mode-line right)))
         (available-width-left
          (- (/
              (- (window-total-width) centre-width)
              2)
             left-width))
         (available-width-right
          (- (window-total-width)
             left-width
             available-width-left
             centre-width
             right-width)))
    (append left
            (list (format (format "%%%ds" available-width-left) ""))
            centre
            (list (format (format "%%%ds" available-width-right) ""))
            right)))

(defvar-local my-mode-line-tab-bar-indicator
  '(:eval
    (let ((tab-name (alist-get 'name (assq 'current-tab (funcall tab-bar-tabs-function)))))
      (propertize
       tab-name
       'help-echo "Tab name"
       'face '(:inherit mode-line-buffer-id)
       'mouse-face 'mode-line-highlight))))
(put 'my-mode-line-tab-bar-indicator 'risky-local-variable t)

(defvar-local my-evil-mode-line-tag
    '(:eval
      (cond ((mode-line-window-selected-p)
             evil-mode-line-tag)
            (t
             (propertize evil-mode-line-tag
                         'face '(:inherit mode-line-inactive :background "light grey" :weight bold))))))
(put 'my-evil-mode-line-tag 'risky-local-variable t)

(defvar-local my-mode-line-buffer-identification
  '(:eval (if (buffer-file-name)
              (propertize (my-ellipsize-file-name
                           (file-name-nondirectory (buffer-file-name))
                           36)
                          'help-echo (abbreviate-file-name (buffer-file-name))
                          'face '(:inherit mode-line-emphasis)
                          'mouse-face 'mode-line-highlight)
            (propertize (buffer-name)
                        'help-echo "Buffer name"
                        'face '(:inherit mode-line-buffer-id)
                        'mouse-face 'mode-line-highlight))))
(put 'my-mode-line-buffer-identification 'risky-local-variable t)

(defvar-local my-mode-line-git-status
  ;; Format: (defun vc-default-mode-line-string (backend file) in vc-hooks.el
  ;;   \"BACKEND-REV\"        if the file is up-to-date
  ;;   \"BACKEND:REV\"        if the file is edited (or locked by the calling user)
  ;;   \"BACKEND:LOCKER:REV\" if the file is locked by somebody else
  ;;   \"BACKEND@REV\"        if the file was locally added
  ;;   \"BACKEND!REV\"        if the file contains conflicts or was removed
  ;;   \"BACKEND?REV\"        if the file is under VC, but is missing

  '(:eval (when vc-mode
            (let* ((git-status (vc-git-mode-line-string (buffer-file-name)))
                   (status (replace-regexp-in-string "^Git" "" git-status))
                   (class (substring-no-properties status 0 1))
                   (locked? (string-match
                             (rx (and
                                  line-start ":"
                                  (one-or-more alnum) ":"
                                  (group (one-or-more alnum))))
                             status))
                   (branch
                    (if locked?  (match-string 1 status)
                      (substring status 1)))
                   (git-mode-line-status (concat " " branch)))
              (cond
               ;; up-to-date
               ((string-equal "-" class)
                (propertize git-mode-line-status
                            'face '(:inherit vc-up-to-date-state :weight bold)
                            'mouse-face 'mode-line-highlight))
               ;; locked
               (locked?
                (propertize git-mode-line-status
                            'face '(:inherit vc-locked-state :weight bold)
                            'mouse-face 'mode-line-highlight))
               ;; edited
               ((string-equal ":" class)
                (propertize git-mode-line-status
                            'face '(:inherit vc-edited-state :weight bold)
                            'mouse-face 'mode-line-highlight))
               ;; locally added
               ((string-equal "@" class)
                (propertize git-mode-line-status
                            'face '(:inherit vc-locally-added-state :weight bold)
                            'mouse-face 'mode-line-highlight))
               ;; removed or conflicting
               ((string-equal "!" class)
                (propertize git-mode-line-status
                            'face '(:inherit vc-removed-state :weight bold)
                            'mouse-face 'mode-line-highlight))
               ;; missing
               ((string-equal "?" class)
                (propertize git-mode-line-status
                            'face '(:inherit vc-missing-state :weight bold)
                            'mouse-face 'mode-line-highlight))
               ((t git-mode-line-status)))))))
(put 'my-mode-line-git-status 'risky-local-variable t)

(defvar-local my-mode-line-centre-placeholder "")
(put 'my-mode-line-centre-placeholder 'risky-local-variable t)

(defvar-local my-mode-line-position
  '(:propertize "(%l,%C)"
                help-echo "(Line,Column)"
                mouse-face mode-line-highlight))
(put 'my-mode-line-position 'risky-local-variable t)

(defvar-local my-mode-line-buffer-size
  '(:propertize "%I"
                help-echo "Size"
                mouse-face mode-line-highlight))
(put 'my-mode-line-buffer-size 'risky-local-variable t)

(defvar-local my-mode-line-modes
  '(:eval (and (or (and (consp mode-name)
                        (setcar mode-name
                                (propertize (car mode-name)
                                            'face '(:inherit mode-line-emphasis))))
                   (setq mode-name
                         (propertize mode-name
                                     'face '(:inherit mode-line-emphasis))))
               minions-mode-line-modes)))
(put 'my-mode-line-modes 'risky-local-variable t)

(defvar-local my-mode-line-percent-position
  '(:eval (let ((p (format-mode-line "%p")))
            (cond
             ((string-equal p "All")
              (propertize "All" 'help-echo p 'mouse-face 'mode-line-highlight))
             ((string-equal p "Top")
              (propertize "Top" 'help-echo p 'mouse-face 'mode-line-highlight))
             ((string-equal p "Bottom")
              (propertize "Bot" 'help-echo p 'mouse-face 'mode-line-highlight))
             (t
              (propertize (concat p  "%%")
                          'help-echo "Position"
                          'mouse-face 'mode-line-highlight))))))
(put 'my-mode-line-percent-position 'risky-local-variable t)

(setq-default mode-line-format
              '(:eval
                (my-mode-line-render
                 ;; left hand side
                 (list
                  "%e"
                  my-evil-mode-line-tag
                  " "
                  my-mode-line-buffer-identification
                  " "
                  my-mode-line-git-status)

                 ;; centre
                 (list
                  my-mode-line-centre-placeholder)

                 ;; right hand side
                 (cond
                  ((mode-line-window-selected-p)
                   (list
                    my-mode-line-buffer-size
                    " "
                    my-mode-line-modes
                    my-mode-line-percent-position
                    " "))
                  (t
                   (list
                    " "
                    my-mode-line-percent-position
                    " "))))))

(provide 'my-mode-line)
