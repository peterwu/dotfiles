;;; my-theme.el -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-completions
   '((matches . (extrabold background intense))
     (selection . (semibold accented intense))
     (popup . (accented))))
  (modus-themes-fringes 'subtle)
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line '(borderless))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-paren-match '(bold intense))
  (modus-themes-prompts '(bold intense))
  (modus-themes-region '(bg-only))
  (modus-themes-syntax '(alt-syntax green-strings yellow-comments))
  (modus-themes-tabs-accented t)
  :config
  ;; Load the theme of my choice: operandi
  (load-theme 'modus-operandi t))

(provide 'my-theme)
