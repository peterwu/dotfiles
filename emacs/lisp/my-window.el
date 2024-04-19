;;; my-window.el -*- lexical-binding: t; -*-

(use-package windmove
  :custom
  (windmove-create-window nil)
  :bind
  (:map my-window-map
        ("<left>"  . windmove-left)
        ("<right>" . windmove-right)
        ("<up>"    . windmove-up)
        ("<down>"  . windmove-down))
  (:repeat-map my-window-repeat-map
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
  (:map my-window-map
        ("u" . winner-undo)
        ("U" . winner-redo))
  (:repeat-map my-window-repeat-map
               ("u" . winner-undo)
               ("U" . winner-redo))
  :config
  (winner-mode +1))

;; window numbering
(defvar my-window-numbering-windows-alist nil)

(defun my-window-numbering-get-number (&optional window)
  (let ((window (or window (selected-window))))
    (car (rassq window my-window-numbering-windows-alist))))

(defun my-window-numbering-select-window (number)
  (let ((window (cdr (assq number my-window-numbering-windows-alist))))
    (when (window-live-p window)
      (select-window window))))

(defun my-window-numbering-update ()
  (setq my-window-numbering-windows-alist
        (let ((result)
              (index 1)
              (windows (window-list nil 0 (frame-first-window))))
          (dolist (window windows result)
            (setq result (append result `(,(cons index window))))
            (setq index (1+ index))))))

(add-hook 'minibuffer-setup-hook 'my-window-numbering-update)
(add-hook 'window-configuration-change-hook
          'my-window-numbering-update)
(dolist (frame (frame-list))
  (select-frame frame)
  (my-window-numbering-update))

(defun my-select-window (&optional number)
  (interactive "p")
  (my-window-numbering-select-window number))

;; define interactive functions for keymap
(dolist (i (number-sequence 1 9))
  (eval `(defun ,(intern (format "my-select-window-%s" i)) ()
           ,(format "Select the window with number %i." i)
           (interactive)
           (my-select-window ,i))))

;; key binds
(bind-key "w" #'my-select-window my-window-map)

(bind-key "1" #'my-select-window-1 my-window-map)
(bind-key "2" #'my-select-window-2 my-window-map)
(bind-key "3" #'my-select-window-3 my-window-map)
(bind-key "4" #'my-select-window-4 my-window-map)
(bind-key "5" #'my-select-window-5 my-window-map)
(bind-key "6" #'my-select-window-6 my-window-map)
(bind-key "7" #'my-select-window-7 my-window-map)
(bind-key "8" #'my-select-window-8 my-window-map)
(bind-key "9" #'my-select-window-9 my-window-map)

(provide 'my-window)
