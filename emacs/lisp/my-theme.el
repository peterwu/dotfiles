;;; my-theme.el -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-completions
   '((matches . (background extrabold intense))
     (selection . (accented intense semibold))
     (popup . (accented))))
  (modus-themes-fringes 'subtle)
  (modus-themes-hl-line '(accented intense))
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-paren-match '(bold intense))
  (modus-themes-prompts '(bold intense))
  (modus-themes-region '(accented bg-only no-extend))
  (modus-themes-subtle-line-numbers t)
  (modus-themes-syntax '(alt-syntax green-strings yellow-comments))
  (modus-themes-tabs-accented t)
  :config
  (load-theme 'modus-operandi t))

(provide 'my-theme)
