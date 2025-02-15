;;; my-window.el -*- lexical-binding: t; -*-

(use-package windmove
  :custom
  (windmove-create-window nil)
  :bind
  (:map my-ctl-z-ctl-w-map
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
  (switch-to-buffer-obey-display-actions t)
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
      (window-height . 11))))
  :hook
  ((help-mode custom-mode) . visual-line-mode))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :bind
  (:map my-ctl-z-ctl-w-map
        ("u" . winner-undo)
        ("U" . winner-redo))
  (:repeat-map my-window-winner-repeat-map
               ("u" . winner-undo)
               ("U" . winner-redo))
  :config
  (winner-mode +1))

;; my-window-numbering
(defvar my-window-numbering--indexed-windows-per-frame-list nil
  "This alist keeps track of the displayed windows on the selected frame
in the following data structure:

'((#frame-1 '((1 . #window-1) (2 . #window-2) ... ))
  (#frame-2 '((1 . #window-1) (2 . #window-2) ... ))
  ... )")

(defun my-window-numbering-get-number (&optional frame window)
  "Return the number of the WINDOW in the FRAME.
If FRAME is omitted, the currently selected frame is assumed;
If WINDOW is omitted, the currently selected window is assumed."
  (let ((frame (or frame (selected-frame)))
        (window (or window (selected-window))))
    (car
     (rassq window
            (cadr
             (assq frame
                   my-window-numbering--indexed-windows-per-frame-list))))))

(defun my-window-numbering-select-window (number &optional frame)
  "Select the window in the FRAME indicated by NUMBER."
  (let* ((frame (or frame (selected-frame)))
         (window (cdr
                  (assq number
                        (cadr (assq frame my-window-numbering--indexed-windows-per-frame-list))))))
    (when (window-live-p window)
      (select-window window))))

(defun my-window-numbering--update ()
  "Update the window numbers per frame."
  (setq my-window-numbering--indexed-windows-per-frame-list
        (seq-map (lambda (frame)
                   (cons frame
                         (list (seq-map-indexed
                                (lambda (elt idx)
                                  (cons (1+ idx) elt))
                                (window-list
                                 frame nil (frame-first-window frame))))))
                 (frame-list))))

(add-hook 'minibuffer-setup-hook
          #'my-window-numbering--update)
(add-hook 'window-configuration-change-hook
          #'my-window-numbering--update)

(defun my-select-window (&optional number)
  "Select window by NUMBER. If NUMBER is omitted, the next window is selected."
  (interactive "P")
  (if (numberp number)
      (my-window-numbering-select-window number)
    (other-window 1)))

(defmacro my-select-window-n-keybind (n)
  "Create my-select-window-N functions and respective key binds, where N
indicates the number assigned to window."
  (let ((my-select-window-n (intern (format "my-select-window-%i" n))))
    `(progn
       (defun ,my-select-window-n ()
         ,(format "Select the window with number %i." n)
         (interactive)
         (my-select-window ,n))

       (bind-keys :map my-ctl-z-ctl-w-map
                  (,(number-to-string n) . ,my-select-window-n)
                  :repeat-map my-window-numbering-repeat-map
                  (,(number-to-string n) . ,my-select-window-n)))))

;; key binds
(bind-keys :map my-ctl-z-ctl-w-map
           ("w" . my-select-window)
           :repeat-map my-window-switch-repeat-map
           ("w" . my-select-window))

(dotimes (n 9)
  (eval
   `(my-select-window-n-keybind ,(1+ n))))

(provide 'my-window)
