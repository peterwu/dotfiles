;;; my-theme.el -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-prompts '(bold intense))

  (modus-themes-common-palette-overrides
   '((bg-mode-line-active bg-inactive)
     (fg-mode-line-active fg-main)
     (bg-mode-line-inactive bg-inactive)
     (fg-mode-line-inactive fg-dim)
     (border-mode-line-active bg-main)
     (border-mode-line-inactive bg-inactive)))
  :config
  (load-theme 'modus-operandi)
  (add-to-list 'modus-operandi-palette '(black "#000000"))
  (add-to-list 'modus-operandi-palette '(white "#ffffff")))

(provide 'my-theme)
