;;; my-packages.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;
;; built-in packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package align
  :preface
  (defun my-align-simple(beg end word)
    (interactive "r\nsSimple align with word: ")
    (let ((regexp (format "\\(\\s-*\\) %s" word))
          (group 1)
          (spacing 1)
          (repeat t))
      (align-regexp beg end regexp group spacing repeat)))

  (defun my-align-complex()
    (interactive)
    (setq current-prefix-arg t)
    (call-interactively #'align-regexp))
  :bind
  (:map my-go-map
        ("l" . my-align-simple)
        ("L" . my-align-complex)))

(use-package autorevert
  :custom
  (auto-revert-verbose t)
  :config
  (global-auto-revert-mode +1))

(use-package battery
  :custom
  (battery-load-low 20)
  (battery-load-critical 10)
  (battery-mode-line-format "[%b%p%%]")
  (battery-mode-line-limit 95)
  (battery-update-interval 180)
  :bind
  (:map my-toggle-map
        ("b" . display-battery-mode))
  :config
  (display-battery-mode -1))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook (text-mode prog-mode)
  :bind
  (:map my-toggle-map
        ("n" . display-line-numbers-mode)))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :config
  (global-eldoc-mode +1))

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

(use-package eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function
   (lambda ()
     (concat
      "┌ "
      (propertize
       (format-time-string "%H:%M" (current-time))
       'face `(:foreground "dark magenta"))
      " "
      (propertize
       (user-login-name)
       'face `(:foreground "dark blue"))
      (propertize
       (concat  "@" (system-name) ":")
       'face `(:foreground "sienna"))
      (propertize
       (abbreviate-file-name (eshell/pwd))
       'face `(:foreground "dark blue" :weight bold))
      "\n"
      "└ "
      (let ((prompt (if (= (user-uid) 0) "#" "$")))
        (if (= eshell-last-command-status 0)
            (propertize prompt 'face `(:foreground "dark green"))
          (propertize prompt 'face `(:foreground "dark red"))))
      " ")))
  :hook
  (eshell-mode . (lambda ()
                   (setq-local global-hl-line-mode nil))))

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
  :bind
  ([remap list-buffers] . ibuffer))

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

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-auto-rescan-maxout 600000)
  (imenu-eager-completion-buffer t)
  (imenu-level-separator "/")
  (imenu-max-item-length 100)
  (imenu-space-replacement " ")
  (imenu-use-markers t)
  (imenu-use-popup-menu nil))

(use-package isearch
  :custom
  (isearch-allow-scroll 'unlimited)
  (isearch-lax-whitespace t)
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-repeat-on-direction-change t)
  (isearch-yank-on-move 'shift)
  (lazy-count-prefix-format "[%s/%s] ")
  (lazy-count-suffix-format nil)
  (search-highlight t)
  (search-whitespace-regexp ".*?"))

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
  :commands proceed
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-descend t)
  (proced-filter 'user))

(use-package project
  :custom
  (project-switch-commands
   '((?f "File" project-find-file)
     (?g "Grep" project-find-regexp)
     (?D "Dired" project-dired)
     (?b "Buffer" project-switch-to-buffer)
     (?r "Query replace" project-query-replace-regexp)
     (?m "Magit" magit-project-status)
     (?e "Eshell" project-eshell))))

(use-package pulse
  :preface
  (defconst my-pulse-duration 0.200)

  (defun my-pulse--cut-advice (orig-fn beg end &rest region)
    (pulse-momentary-highlight-region beg end)
    (sit-for my-pulse-duration)
    (apply orig-fn beg end region))

  (defun my-pulse--copy-advice (orig-fn beg end &rest region)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end region))

  (defun my-pulse--paste-advice (orig-fn &rest args)
    (let (beg end)
      (setq beg (point))
      (apply orig-fn args)
      (setq end (point))
      (pulse-momentary-highlight-region beg end)))
  :config
  (dolist (construct '((kill-region             . my-pulse--cut-advice)
                       (kill-ring-save          . my-pulse--copy-advice)
                       (yank                    . my-pulse--paste-advice)

                       (my-cut-to-clipboard     . my-pulse--cut-advice)
                       (my-copy-to-clipboard    . my-pulse--copy-advice)
                       (my-paste-from-clipboard . my-pulse--paste-advice)

                       (evil-delete             . my-pulse--cut-advice)
                       (evil-yank               . my-pulse--copy-advice)
                       (evil-paste-after        . my-pulse--paste-advice)
                       (evil-paste-before       . my-pulse--paste-advice)))
    (advice-add (car construct) :around (cdr construct))))

(use-package recentf
  :preface
  (defun my-recentf-open-other-window (file)
    "Prompt for FILE in `recentf-list' and visit it in other window.
Enable `recentf-mode' if it isn't already."
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read (format-prompt "Open recent file in other window" nil)
                              recentf-list nil t))))
    (when file
      (funcall #'find-file-other-window file)))

  (defun my-recentf-open-other-tab (file)
    "Prompt for FILE in `recentf-list' and visit it in other tab.
Enable `recentf-mode' if it isn't already."
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read (format-prompt "Open recent file in other tab" nil)
                              recentf-list nil t))))
    (when file
      (funcall #'find-file-other-tab file)))

  (defun my-recentf-open-other-frame (file)
    "Prompt for FILE in `recentf-list' and visit it in other frame.
Enable `recentf-mode' if it isn't already."
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read (format-prompt "Open recent file in other frame" nil)
                              recentf-list nil t))))
    (when file
      (funcall #'find-file-other-frame file)))
  :custom
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (recentf-max-saved-items 50)
  :bind
  (:map my-ctl-z-map
        ("C-r" . recentf-open))
  (:map my-ctl-z-4-map
        ("C-r" . my-recentf-open-other-window))
  (:map my-ctl-z-tab-map
        ("C-r" . my-recentf-open-other-tab))
  (:map my-ctl-z-5-map
        ("C-r" . my-recentf-open-other-frame))
  :config
  (recentf-mode +1))

(use-package repeat
  :config
  (repeat-mode +1))

(use-package savehist
  :custom
  (history-delete-duplicates t)
  (history-length 1000)
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode +1))

(use-package saveplace
  :custom
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
  (:map my-toggle-map
        ("t" . global-tab-line-mode))
  :config
  (global-tab-line-mode -1))

(use-package term
  :custom
  (explicit-shell-file-name "/bin/bash --login")
  :bind
  ([f12] . (lambda () (interactive)
             (term explicit-shell-file-name)))
  :hook
  (term-mode . (lambda()
                 (setq-local global-hl-line-mode nil)
                 (term-set-escape-char ?\C-x))))

(use-package term/xterm
  :unless (display-graphic-p)
  :custom
  (xterm-extra-capabilities '(getSelection setSelection))
  :config
  (terminal-init-xterm))

(use-package time
  :commands world-clock
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
  (:map my-toggle-map
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
  (tramp-default-method "sshx"))

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t))

(use-package whitespace
  :custom
  (whitespace-line-column fill-column)
  (whitespace-style '(face lines-char trailing))
  :hook
  (before-save . whitespace-cleanup)
  ((conf-mode prog-mode text-mode) . whitespace-mode)
  :bind
  (:map my-toggle-map
        ("w" . whitespace-mode)))

(use-package xt-mouse
  :unless (display-graphic-p)
  :bind
  ("<mouse-4>" . scroll-down-line)
  ("<mouse-5>" . scroll-up-line)
  :config
  (xterm-mouse-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 3rd party packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package goto-chg
  :ensure t)

(use-package keycast
  :ensure t
  :custom
  (keycast-mode-line-format "%k%c%R")
  (keycast-mode-line-insert-after 'my-mode-line-centre-place-holder)
  (keycast-mode-line-remove-tail-elements nil)
  :bind
  (:map my-toggle-map
        ("k" .  keycast-mode-line-mode)))

(use-package magit
  :ensure t
  :bind
  (:map my-magit-map
        ("g" . magit-status)
        ("j" . magit-dispatch)
        ("J" . magit-file-dispatch)
        ("r" . vc-refresh-state)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(provide 'my-packages)
