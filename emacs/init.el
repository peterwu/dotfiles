;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (locate-user-emacs-file "mine"))

(use-package my-package)

(use-package my-defaults)
(use-package my-msft)
(use-package my-mode-line)
(use-package my-theme)
(use-package my-dired)
(use-package my-evil)
(use-package my-gnus)
(use-package my-org)
(use-package my-window)

;; small packages
(use-package autorevert
  :custom
  (auto-revert-verbose t)
  :config
  (global-auto-revert-mode +1))

(use-package avy
  :ensure t
  :after evil
  :custom
  (avy-background t)
  :bind
  (:map isearch-mode-map
        ("C-'" . avy-isearch))
  (:map my-evil-jump-map
        ("f" .  avy-goto-char)
        ("r" .  avy-resume)
        ("s" .  avy-goto-char-2)
        ("j" .  avy-goto-char-timer)
        ("w" .  avy-goto-word-1)
        ("W" .  avy-goto-word-0)))

(use-package battery
  :after evil
  :custom
  (battery-load-low 20)
  (battery-load-critical 10)
  (battery-mode-line-format "[%b%p%%]")
  (battery-mode-line-limit 95)
  (battery-update-interval 180)
  :bind
  (:map my-evil-toggle-map
        ("b" . display-battery-mode))
  :config
  (display-battery-mode -1))

(use-package clang-format
  :if (file-executable-p "/usr/bin/clang-format")
  :preface (defvar my-clang-format-style "file")
  :preface (defvar my-clang-format-fallback-style
             (concat "{"
                     "BasedOnStyle: LLVM,"
                     "Language: Cpp,"

                     "AccessModifierOffset: -4,"
                     "IndentWidth: 4,"
                     "UseTab: Never,"

                     "AllowShortFunctionsOnASingleLine: Empty,"
                     "AllowShortLambdasOnASingleLine: Empty,"
                     "AlwaysBreakTemplateDeclarations: Yes,"
                     "BreakBeforeBraces: Stroustrup,"
                     "BreakConstructorInitializers: BeforeComma,"
                     "IndentPPDirectives: AfterHash,"
                     "PointerAlignment: Left"
                     "}"))
  :hook
  ((c-mode c++-mode) .
   (lambda ()
     (add-hook 'before-save-hook
               (lambda ()
                 (if (locate-dominating-file "." ".clang-format")
                     (clang-format-buffer my-clang-format-style)
                   (clang-format-buffer my-clang-format-fallback-style))
                 nil)
               nil
               t))))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-scroll-margin 5)
  :config
  (global-corfu-mode +1))

(use-package display-line-numbers
  :after evil
  :custom
  (display-line-numbers-type 'relative)
  :hook (text-mode prog-mode)
  :bind
  (:map my-evil-toggle-map
        ("n" . display-line-numbers-mode)))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eglot
  :hook
  ((c-mode
    c++-mode
    python-mode) . eglot-ensure))

(use-package eldoc
  :hook
  (emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))

(use-package electric
  :custom
  (electric-pair-pairs
   '((8216 . 8217)
     (8220 . 8221)
     (171 . 187)))
  (electric-pair-preserve-balance t)
  (electric-pair-skip-whitespace nil)
  (electric-pair-skip-whitespace-chars
   '(9
     10
     32))

  (electric-quote-context-sensitive t)
  (electric-quote-paragraph t)
  (electric-quote-replace-double t)
  (electric-quote-string nil)
  :config
  (electric-indent-mode +1)
  (electric-pair-mode +1)
  (electric-quote-mode +1))

(use-package erc
  :custom
  (erc-nick "ixlxi")
  (erc-user-full-name "iExcel")
  (erc-server "irc.libera.chat")
  (erc-port "6697"))

(use-package eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function
   (lambda ()
     (concat
      "┌ "
      (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "dark magenta"))
      " "
      (propertize (user-login-name)  'face `(:foreground "dark blue"))
      (propertize (concat  "@" (system-name) ":") 'face `(:foreground "sienna"))
      (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground "dark blue" :weight bold))
      "\n"
      "└ "
      (let ((prompt (if (= (user-uid) 0) "#" "$")))
        (if (= eshell-last-command-status 0)
            (propertize prompt 'face `(:foreground "dark green"))
          (propertize prompt 'face `(:foreground "dark red"))))
      " ")))
  :bind
  ("<f12>" . eshell)
  :hook
  (eshell-mode . (lambda ()
                   (setq-local global-hl-line-mode nil))))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-suppress-zero-counters t)
  (flymake-wrap-around nil)
  :bind
  ("C-c ! s" . flymake-start)
  ("C-c ! d" . flymake-show-diagnostics-buffer)
  ("C-c ! n" . flymake-goto-next-error)
  ("C-c ! p" . flymake-goto-prev-error))

(use-package ispell
  :unless (eq system-type 'windows-nt)
  :if (locate-file "hunspell" exec-path exec-suffixes 1)
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US"))

(use-package flyspell
  :unless (eq system-type 'windows-nt)
  :after ispell
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-restore-window-configuration-after-quit t)
  (gdb-show-main t))

(use-package goto-chg
  :ensure t)

(use-package ibuffer
  :custom
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-display-summary nil)
  (ibuffer-expert t)
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 30 30 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
  (ibuffer-movement-cycle nil)
  (ibuffer-old-time 48)
  (ibuffer-saved-filter-groups nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-use-header-line t)
  (ibuffer-use-other-window nil)
  :hook
  (ibuffer-mode . #'hl-line-mode))

(use-package icomplete
  :custom
  (icomplete-hide-common-prefix nil)
  (icomplete-in-buffer t)
  (icomplete-separator (propertize " · " 'face 'shadow))
  (icomplete-show-matches-on-no-input t)
  (icomplete-tidy-shadowed-file-names t)
  (icomplete-with-completion-tables t)
  :config
  (fido-vertical-mode +1))

(use-package ido
  :custom
  (ido-default-buffer-method 'selected-window)
  (ido-default-file-method 'selected-window)
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-max-prospects 8)
  (ido-save-directory-list-file
   (expand-file-name "cache/ido.last" user-emacs-directory))
  (ido-show-dot-for-dired t)
  (ido-use-virtual-buffers t)
  :hook
  (ido-minibuffer-setup .
                        (lambda ()
                          (setf (nth 0 ido-decorations) "\n")
                          (setf (nth 1 ido-decorations) "")
                          (setf (nth 2 ido-decorations)
                                (propertize "\n" 'face 'shadow))
                          (setf (nth 3 ido-decorations)
                                (propertize
                                 (concat "\n"
                                         (if (char-displayable-p ?…) "…" "..."))
                                 'face 'shadow))
                          (setf (nth 4 ido-decorations)
                                (propertize "\n" 'face 'shadow))
                          (setf (nth 5 ido-decorations)
                                (propertize "" 'face 'shadow))))
  :config
  (ido-mode +1))

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-auto-rescan-maxout 600000)
  (imenu-eager-completion-buffer t)
  (imenu-level-separator "/")
  (imenu-max-item-length 100)
  (imenu-space-replacement " ")
  (imenu-use-markers t)
  (imenu-use-popup-menu nil)
  :bind
  ("M-i" . imenu))

(use-package isearch
  :custom
  (isearch-allow-scroll 'unlimited)
  (isearch-lax-whitespace t)
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-yank-on-move 'shift)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " (%s/%s)")
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  :bind
  ("C-g" . isearch-cancel)
  ("M-/" . isearch-complete)
  (:map minibuffer-local-isearch-map
        ("M-/" . isearch-complete-edit)))

(use-package keycast
  :ensure t
  :after evil
  :custom
  (keycast-mode-line-format "%k%c%R")
  (keycast-mode-line-insert-after 'my-mode-line-centre-placeholder)
  (keycast-mode-line-remove-tail-elements nil)
  :bind
  (:map my-evil-toggle-map
        ("k" .  keycast-mode)))

(use-package magit
  :ensure t
  :after evil
  :bind
  ("C-x g" . magit-status)
  (:map my-evil-magit-map
        ("g" . magit-status)
        ("j" . magit-dispatch)
        ("J" . magit-file-dispatch)
        ("r" . vc-refresh-state)))

(use-package man
  :custom
  (Man-notify-method 'bully)
  :bind
  ("Q" . (lambda () (interactive)
           (delete-other-frames)
           (delete-frame))))

(use-package minions
             :ensure t
  :custom
  (minions-mode-line-delimiters '("" . ""))
  (minions-mode-line-face 'mode-line-emphasis)
  (minions-mode-line-lighter (if (char-displayable-p ?…) "…" "...")))

(use-package mouse
  :if (display-graphic-p)
  :custom
  (make-pointer-invisible t)
  (mouse-drag-copy-region nil)
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-scroll-amount
   '(1
     ((shift) . 5)
     ((meta) . 0.5)
     ((control) . text-scale)))
  :config
  (mouse-wheel-mode +1))

(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery nil)
  (show-paren-when-point-inside-paren nil)
  :config
  (show-paren-mode +1))

(use-package proced
  :autoload proceed
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-descend t)
  (proced-filter 'user))

(use-package project
  :custom
  (project-list-file (expand-file-name "cache/projects" user-emacs-directory))
  (project-switch-commands
   '((?f "File" project-find-file)
     (?g "Grep" project-find-regexp)
     (?D "Dired" project-dired)
     (?b "Buffer" project-switch-to-buffer)
     (?r "Query replace" project-query-replace-regexp)
     (?m "Magit" magit-project-status)
     (?e "Eshell" project-eshell))))

(use-package rainbow-mode
  :ensure t
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil)
  :hook prog-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

(use-package recentf
  :custom
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (recentf-max-saved-items 200)
  (recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  :config
  (recentf-mode +1))

(use-package savehist
  :custom
  (history-delete-duplicates t)
  (history-length 1000)
  (savehist-file (expand-file-name "cache/savehist" user-emacs-directory))
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode +1))

(use-package saveplace
  :custom
  (save-place-file (expand-file-name "cache/places" user-emacs-directory))
  (save-place-forget-unreadable-files t)
  :config
  (save-place-mode +1))

(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-choice t)
  (tab-bar-new-tab-to 'right)
  (tab-bar-position nil)
  (tab-bar-show nil)
  (tab-bar-tab-hints nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all)
  :config
  (tab-bar-mode -1)
  (tab-bar-history-mode -1))

(use-package tab-line
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-exclude-modes '(completion-list-mode
                            dired-sidebar-mode
                            help-mode))
  :bind
  (:map my-evil-toggle-map
        ("t" . global-tab-line-mode ))
  :config
  (global-tab-line-mode -1))

(use-package time
  :autoload world-clock
  :custom
  (display-time-default-load-average nil)
  (display-time-format "[%H:%M]")
  (display-time-interval 60)

  (zoneinfo-style-world-list
   '(("America/Los_Angeles" "Los Angeles")
     ("America/New_York" "New York")
     ("Europe/Brussels" "Brussels")
     ("Asia/Shanghai" "Shanghai")
     ("Asia/Tokyo" "Tokyo")))

  (world-clock-buffer-name "*world-clock*")
  (world-clock-list t)
  (world-clock-timer-enable t)
  (world-clock-time-format "%R %z  %A %d %B")
  (world-clock-timer-second 60)
  :bind
  (:map my-evil-toggle-map
        ("c" . display-time-mode)
        ("g" . world-clock))
  :config
  (display-time-mode -1))

(use-package tooltip
  :custom
  (tooltip-delay 0.5)
  (tooltip-frame-parameters
   '((name . "tooltip")
     (internal-border-width . 6)
     (border-width . 0)
     (no-special-glyphs . t)))
  (tooltip-short-delay 0.5)
  (x-gtk-use-system-tooltips nil)
  :config
  (tooltip-mode +1))

(use-package tramp
  :custom
  (tramp-default-method "sshx")
  (tramp-persistency-file-name
   (expand-file-name "cache/tramp" user-emacs-directory)))

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package whitespace
  :hook
  (before-save . whitespace-cleanup)
  :bind
  (:map my-evil-toggle-map
        ("w" . whitespace-mode)))

(use-package xt-mouse
  :unless (display-graphic-p)
  :bind
  ("<mouse-4>" . scroll-down-line)
  ("<mouse-5>" . scroll-up-line)
  :config
  (xterm-mouse-mode +1))

(provide 'init)
;;; init.el ends here
