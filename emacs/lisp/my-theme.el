;;; my-theme.el -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-prompts '(bold intense))

  ;; emacs 29
  (modus-themes-mode-line '(borderless))

  ;; emacs 30
  (modus-themes-common-palette-overrides
   `((bg-mode-line-active bg-inactive)
     (fg-mode-line-active fg-main)
     (bg-mode-line-inactive bg-inactive)
     (fg-mode-line-inactive fg-dim)
     (border-mode-line-active bg-main)
     (border-mode-line-inactive bg-inactive)))
  :config
  (load-theme 'modus-operandi t))

(provide 'my-theme)
