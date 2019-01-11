(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode t))

(use-package evil-surround
  :config  
  (global-evil-surround-mode t))

(use-package evil-goggles
  :config  
  (setq evil-goggles-pulse t)
  (setq evil-goggles-duration 1.000)
  (evil-goggles-mode))

(use-package evil-matchit
  :config  
  (global-evil-matchit-mode t))

(use-package evil-quickscope
  :config
  (global-evil-quickscope-mode t))

(use-package yaml-mode)

(use-package nov 
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (add-hook 'nov-mode-hook (lambda ()
			     (face-remap-add-relative 'default :height 1.2)
			     (display-line-numbers-mode -1))))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode t))

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package org
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))
