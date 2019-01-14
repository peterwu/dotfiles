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
  :init
  (setq auto-package-update-last-update-day-filename (expand-file-name "cache/last-package-update-day" user-emacs-directory))
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
  :init
  (setq nov-save-place-file (expand-file-name "cache/nov-places" user-emacs-directory))
  :config
  (add-hook 'nov-mode-hook (lambda ()
			     (face-remap-add-relative 'default :height 1.2)
			     (display-line-numbers-mode -1))))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package ido
  :init
  (setq ido-save-directory-list-file (expand-file-name "cache/ido.last" user-emacs-directory))
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

(use-package org-evil)

(use-package projectile
  :init
  (setq projectile-cache-file (expand-file-name "cache/projectile.cache" user-emacs-directory)
	projectile-known-projects-file (expand-file-name "cache/projectile-bookmarks.eld" user-emacs-directory))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-completion-system 'ido))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name "cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ([f8]        . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))
