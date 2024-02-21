;;; my-window.el -*- lexical-binding: t; -*-

(with-package 'windmove
  (setq windmove-create-window nil)
  (with-eval-after-load 'evil
    (define-key evil-window-map (kbd "<left>")  #'windmove-left)
    (define-key evil-window-map (kbd "<right>") #'windmove-right)
    (define-key evil-window-map (kbd "<up>")    #'windmove-up)
    (define-key evil-window-map (kbd "<down>")  #'windmove-down))
  (windmove-default-keybindings))

(with-package 'window
  (setq even-window-sizes 'height-only)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq window-combination-resize t)
  (setq window-sides-vertical nil)

  (add-hook 'help-mode-hook   #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode))

(with-package 'winner
  (with-eval-after-load 'evil
    (define-key evil-window-map (kbd "u") #'winner-undo)
    (define-key evil-window-map (kbd "U") #'winner-redo)

    (winner-mode +1)))

(provide 'my-window)
