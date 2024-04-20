;;; my-mode-line.el -*- lexical-binding: t; -*-

;; mode-line
(defun my-ellipsize-file-name (file-name max-length)
  "Ellipsize FILE-NAME if its length exceeds MAX-LENGTH."
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

(defvar-local my-mode-line-window-status-tag
    '(:eval
      (let ((tag (concat " "
                         (number-to-string (my-window-numbering-get-number))
                         " "))
            (selected (mode-line-window-selected-p))
            (dedicated (window-dedicated-p)))
        (cond
         ((and selected dedicated)
          (propertize tag 'face '(:inherit modus-themes-active-red :weight bold)))
         ((and selected (not dedicated))
          (propertize tag 'face '(:inherit modus-themes-active-blue :weight bold)))
         ((and (not selected) dedicated)
          (propertize tag 'face '(:inherit modus-themes-subtle-red)))
         ((and (not selected) (not dedicated))
          (propertize tag 'face '(:inherit modus-themes-subtle-blue))))))
  "Return the status for window.
Active red suggests the window is both selected and dedicated;
Active blue suggests the window is selected but not dedicated;
Subtle red suggests the window is not selected but dedicated;
Subtle blue suggests the window is neither selected nor dedicated.")
(put 'my-mode-line-window-status-tag 'risky-local-variable t)

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
                          'mouse-face 'mode-line-highlight)))
  "Return an enhanced buffer-identification with ellipsized file name when the file name is too long.")
(put 'my-mode-line-buffer-identification 'risky-local-variable t)

(defvar-local my-mode-line-vc-mode
    ;; Format: (defun vc-default-mode-line-string (backend file) in vc-hooks.el
    ;;   \"BACKEND-REV\"        if the file is up-to-date
    ;;   \"BACKEND:REV\"        if the file is edited (or locked by the calling user)
    ;;   \"BACKEND:LOCKER:REV\" if the file is locked by somebody else
    ;;   \"BACKEND@REV\"        if the file was locally added
    ;;   \"BACKEND!REV\"        if the file contains conflicts or was removed
    ;;   \"BACKEND?REV\"        if the file is under VC, but is missing

    '(:eval (when vc-mode
              (let* ((mode (substring-no-properties vc-mode))
                     (status (replace-regexp-in-string "^ Git" "" mode))
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
                 ((t git-mode-line-status))))))
  "Return git status.")
(put 'my-mode-line-vc-mode 'risky-local-variable t)

(defvar-local my-mode-line-centre-place-holder ""
  "Serve as a place holder for centrally aligned mode-line elements.")
(put 'my-mode-line-centre-place-holder 'risky-local-variable t)

(defvar-local my-mode-line-buffer-size
    '(:propertize "%I"
                  help-echo "Size"
                  mouse-face mode-line-highlight)
  "Return the size of the buffer.")
(put 'my-mode-line-buffer-size 'risky-local-variable t)

(defvar-local my-mode-line-modes
    '(:eval (and (or (and (consp mode-name)
                          (setcar mode-name
                                  (propertize (car mode-name)
                                              'face '(:inherit mode-line-emphasis))))
                     (setq mode-name
                           (propertize mode-name
                                       'face '(:inherit mode-line-emphasis))))
                 minions-mode-line-modes))
  "Return the modes information by utilizing the minions package.")
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
                            'mouse-face 'mode-line-highlight)))))
  "Return a slightly modified position information where Bottom is renamed to Bot.")
(put 'my-mode-line-percent-position 'risky-local-variable t)

(setopt mode-line-format
        '(:eval
          (my-mode-line-render
           ;; left
           (list
            my-mode-line-window-status-tag
            " "
            mode-line-mule-info
            mode-line-client
            mode-line-modified
            mode-line-remote
            " "
            my-mode-line-buffer-identification
            " "
            my-mode-line-vc-mode)

           ;; centre
           (list
            my-mode-line-centre-place-holder)

           ;; right
           (when (mode-line-window-selected-p)
             (list
              mode-line-misc-info
              my-mode-line-buffer-size
              " "
              my-mode-line-modes
              my-mode-line-percent-position
              " ")))))

(provide 'my-mode-line)
