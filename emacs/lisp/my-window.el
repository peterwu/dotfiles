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

(provide 'my-window)
