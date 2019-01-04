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

(use-package evil-matchit
  :config  
  (global-evil-matchit-mode t))

(use-package evil-quickscope
  :config
  (global-evil-quickscope-mode t))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (add-to-list 'evil-emacs-state-modes 'nov-mode)
  (add-hook 'nov-mode-hook (lambda ()
			     (face-remap-add-relative 'default :height 1.2)
			     (display-line-numbers-mode -1))))

(use-package magit)

(use-package evil-magit)

(use-package vue-mode)

(use-package helm)

(use-package powerline
  :config
  (powerline-default-theme))

