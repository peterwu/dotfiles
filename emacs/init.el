(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq inhibit-startup-screen t)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq vc-follow-symlinks nil)
(global-hl-line-mode t)

(load-theme 'dracula t)

(setq evil-want-keybinding nil)
(evil-mode t)
(evil-collection-init)
(evil-commentary-mode t)
(global-evil-surround-mode t)
(global-evil-matchit-mode t)
(global-evil-quickscope-mode t)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'evil-emacs-state-modes 'nov-mode)
(add-hook 'nov-mode-hook (lambda ()
  (face-remap-add-relative 'default :height 1.2)
  (display-line-numbers-mode -1)))
