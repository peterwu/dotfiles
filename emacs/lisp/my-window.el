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
  :preface
  (defun my-speedbar-buffer-live-p (buffer &rest args)
    (and (boundp 'speedbar-buffer)
         speedbar-buffer
         (buffer-live-p speedbar-buffer)))
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
      (window-height . 11))

     (my-speedbar-buffer-live-p
      display-buffer-use-some-window)))
  :hook
  ((help-mode custom-mode) . visual-line-mode))

(use-package winner
  :demand t
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

(use-package my-window-numbering)

(provide 'my-window)
