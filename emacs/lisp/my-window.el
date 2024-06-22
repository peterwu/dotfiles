;;; my-window.el -*- lexical-binding: t; -*-

(use-package windmove
  :custom
  (windmove-create-window nil)
  :bind
  (:map my-ctl-z-w-map
        ("<left>"  . windmove-left)
        ("<right>" . windmove-right)
        ("<up>"    . windmove-up)
        ("<down>"  . windmove-down))
  (:repeat-map my-window-windmove-repeat-map
               ("<left>"  . windmove-left)
               ("<right>" . windmove-right)
               ("<up>"    . windmove-up)
               ("<down>"  . windmove-down)))

(use-package window
  :custom
  (even-window-sizes 'height-only)
  (switch-to-buffer-in-dedicated-window 'pop)
  (window-combination-resize t)
  (window-sides-vertical nil)
  :custom
  (display-buffer-alist
   '(((or (major-mode . Info-mode)
          (major-mode . help-mode))
      (display-buffer-reuse-window
       display-buffer-at-bottom)
      (reusable-frames . visible)
      (window-height . 0.31))

     ((major-mode . term-mode)
      (display-buffer-reuse-window
       display-buffer-below-selected)
      (dedicated . t)
      (window-height . 11))

     ((derived-mode .  prog-mode)
      (display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-pop-up-window)
      (mode . prog-mode))))
  :hook
  ((help-mode custom-mode) . visual-line-mode))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :bind
  (:map my-ctl-z-w-map
        ("u" . winner-undo)
        ("U" . winner-redo))
  (:repeat-map my-window-winner-repeat-map
               ("u" . winner-undo)
               ("U" . winner-redo))
  :config
  (winner-mode +1))

;; my-window-numbering
(defvar my-window-numbering--indexed-windows-alist nil
  "This alist keeps track of the displayed windows on the selected frame
in the form of ((index . #window-name) ... ), where index begins at 1.")

(defun my-window-numbering-get-number (&optional window)
  "Return the number of the WINDOW. If WINDOW is omitted, the currently selected
window is assumed."
  (let ((window (or window (selected-window))))
    (car (rassq window my-window-numbering--indexed-windows-alist))))

(defun my-window-numbering-select-window (number)
  "Select the window indicated by NUMBER."
  (let ((window (cdr (assq number my-window-numbering--indexed-windows-alist))))
    (when (window-live-p window)
      (select-window window))))

(defun my-window-numbering--update ()
  "Update the window numbers."
  (setq my-window-numbering--indexed-windows-alist
        (seq-map-indexed (lambda (elt idx)
                           (cons (1+ idx) elt))
                         (window-list nil 0 (frame-first-window)))))

(add-hook 'minibuffer-setup-hook
          #'my-window-numbering--update)
(add-hook 'window-configuration-change-hook
          #'my-window-numbering--update)

(dolist (frame (frame-list))
  (select-frame frame)
  (my-window-numbering--update))

(defun my-select-window (&optional number)
  "Select window by NUMBER. If NUMBER is omitted, the next window is selected."
  (interactive "P")
  (if (numberp number)
      (my-window-numbering-select-window number)
    (other-window 1)))

(defmacro my-select-window-n-keybind (n)
  "Create my-select-window-N functions and respective key binds, where N
indicates the number assigned to window."
  (let ((my-select-window-n (intern (format "my-select-window-%s" n))))
    `(progn
       (defun ,my-select-window-n ()
         ,(format "Select the window with number %i." n)
         (interactive)
         (my-select-window ,n))

       (bind-keys :map my-ctl-z-w-map
                  (,(number-to-string n) . ,my-select-window-n)
                  :repeat-map my-window-numbering-repeat-map
                  (,(number-to-string n) . ,my-select-window-n)))))

;; key binds
(bind-keys :map my-ctl-z-w-map
           ("w" . my-select-window)
           :repeat-map my-window-switch-repeat-map
           ("w" . my-select-window))

(dolist (n (number-sequence 1 9))
  (eval
   `(my-select-window-n-keybind ,n)))

(provide 'my-window)
