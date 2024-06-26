;;; my-theme.el -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-prompts '(bold intense))

  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)))
  :config
  (load-theme 'modus-operandi)
  (add-to-list 'modus-operandi-palette '(black "#000000"))
  (add-to-list 'modus-operandi-palette '(white "#ffffff")))

(provide 'my-theme)
