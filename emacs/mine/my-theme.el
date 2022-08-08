;;; my-theme.el -*- lexical-binding: t; -*-

(setq modus-themes-bold-constructs t)
(setq modus-themes-completions
      '((matches . (extrabold background intense))
        (selection . (semibold accented intense))
        (popup . (accented))))
(setq modus-themes-fringes 'subtle)
(setq modus-themes-italic-constructs t)
(setq modus-themes-mode-line '(borderless))
(setq modus-themes-org-blocks 'tinted-background)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-prompts '(bold intense))
(setq modus-themes-region '(bg-only))
(setq modus-themes-syntax '(alt-syntax green-strings yellow-comments))
(setq modus-themes-tabs-accented t)

;; Load the theme of my choice: operandi
(load-theme 'modus-operandi t)

(provide 'my-theme)
