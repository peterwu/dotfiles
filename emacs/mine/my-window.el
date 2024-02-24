;;; my-window.el -*- lexical-binding: t; -*-

(use-package windmove
  :after evil
  :custom
  (windmove-create-window nil)
  :bind
  (:map evil-window-map
        ("<left>" . windmove-left)
        ("<right>" . windmove-right)
        ("<up>" . windmove-up)
        ("<down>" . windmove-down))
  :config
  (windmove-default-keybindings 'control))

(use-package window
  :custom
  (even-window-sizes 'height-only)
  (switch-to-buffer-in-dedicated-window 'pop)
  (window-combination-resize t)
  (window-sides-vertical nil)
  :hook
  ((help-mode custom-mode) . visual-line-mode))

(use-package winner
  :after evil
  :bind
  (:map evil-window-map
        ("u" . winner-undo)
        ("U" . winner-redo))
  :config
  (winner-mode +1))

(provide 'my-window)
