;;; my-packages.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;
;; built-in packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package align
  :preface
  (defun my-align-squeeze-spaces (beg end regex type)
    (save-excursion
      (let ((line-count (count-lines beg end)))
        (goto-char beg)
        (dotimes (_ line-count)
          (beginning-of-line)

          ;; look for 2 or more spaces
          (let ((spaces-regex "\\([ ]\\{2,\\}\\)")) ;; match 2 or more spaces
            (while (re-search-forward regex (point-at-eol) t)
              (when (save-excursion
                      (cond
                       ;; for type 'right, match spaces after the regex
                       ((eq type 'right)
                        (looking-at spaces-regex))
                       ;; for type 'left, match spaces before the regex
                       ((eq type 'left)
                        (goto-char (match-beginning 0))
                        (looking-back spaces-regex (pos-bol)))))
                (replace-match " "))))

          (forward-line)))))

  (defun my-align(beg end word type)
    (save-restriction
      (narrow-to-region beg end)

      (let ((regexp (if (eq type 'left)
                        (format "\\(\\)%s" word)
                      (format "%s\\(\\)" word)))
            (group 1)
            (spacing 0)
            (repeat nil))
        (my-align-squeeze-spaces (point-min) (point-max) regexp type)
        (align-regexp (point-min) (point-max) regexp group spacing repeat))))

  (defun my-align-left (beg end word)
    (interactive "r\nsAlign with: ")
    (my-align beg end word 'left))

  (defun my-align-right (beg end word)
    (interactive "r\nsAlign with: ")
    (my-align beg end word 'right))
  :custom
  (align-default-spacing 0)
  (align-indent-before-aligning t)
  :bind
  (:map my-ctl-z-g-map
        ("l" . my-align-left)
        ("L" . my-align-right)))

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
  :bind
  (:map my-ctl-z-t-map
        ("b" . display-battery-mode))
  :config
  (display-battery-mode -1))

(use-package delsel
  :config
  (delete-selection-mode +1))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook (text-mode prog-mode)
  :bind
  (:map my-ctl-z-t-map
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
     (modus-themes-with-colors
       (concat
        "┌ "
        (propertize
         (format-time-string "%H:%M" (current-time))
         'face `(:foreground ,magenta))
        " "
        (propertize
         (user-login-name)
         'face `(:foreground ,blue))
        (propertize
         (concat  "@" (system-name) ":")
         'face `(:foreground ,black))
        (propertize
         (abbreviate-file-name (eshell/pwd))
         'face `(:foreground ,blue :weight bold))
        "\n"
        "└ "
        (let ((prompt (if (= (user-uid) 0) "#" "$")))
          (if (= eshell-last-command-status 0)
              (propertize prompt 'face `(:foreground ,green))
            (propertize prompt 'face `(:foreground ,red))))
        " "))))
  :hook
  (eshell-mode . (lambda ()
                   (setq-local global-hl-line-mode nil))))

(use-package frame
  :custom
  (frame-title-format
   '((:eval
      (if (buffer-file-name)
          (abbreviate-file-name (buffer-file-name))
        "%b"))))

  (blink-cursor-blinks 20)
  (blink-cursor-delay 0.2)
  (blink-cursor-interval 0.5)
  (cursor-in-non-selected-windows 'hollow)
  :config
  (blink-cursor-mode +1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

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
  (:map my-ctl-z-map
        ("C-b" . ibuffer)))

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

(use-package minibuffer
  :custom
  (completion-cycle-threshold 3)
  (completions-detailed t)
  (completions-format 'one-column)
  (completion-styles '(initials partial-completion flex))
  (enable-recursive-minibuffers t)
  (minibuffer-eldef-shorten-default t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (resize-mini-windows t)
  :config
  (minibuffer-depth-indicate-mode +1)
  (minibuffer-electric-default-mode +1))

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

(use-package pixel-scroll
  :custom
  (pixel-scroll-precision-interpolate-page t)

  (scroll-conservatively 10000)
  (scroll-margin 1)
  (scroll-preserve-screen-position t)
  (scroll-step 1)
  :config
  (pixel-scroll-precision-mode +1))

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
             (completing-read
              (format-prompt "Open recent file in other window" nil)
              recentf-list nil t))))
    (when file
      (funcall #'find-file-other-window file)))

  (defun my-recentf-open-other-tab (file)
    "Prompt for FILE in `recentf-list' and visit it in other tab.
Enable `recentf-mode' if it isn't already."
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read
              (format-prompt "Open recent file in other tab" nil)
              recentf-list nil t))))
    (when file
      (funcall #'find-file-other-tab file)))

  (defun my-recentf-open-other-frame (file)
    "Prompt for FILE in `recentf-list' and visit it in other frame.
Enable `recentf-mode' if it isn't already."
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read
              (format-prompt "Open recent file in other frame" nil)
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
  (:map my-ctl-z-t-map
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

(use-package simple
  :custom
  (fill-column 80)
  (indent-tabs-mode nil)
  :hook
  (text-mode . auto-fill-mode)
  :config
  (column-number-mode      +1)
  (global-visual-line-mode +1)
  (prettify-symbols-mode   +1)
  (size-indication-mode    +1))

(use-package so-long
  :config
  (global-so-long-mode +1))

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
  (:map my-ctl-z-t-map
        ("t" . global-tab-line-mode))
  :config
  (global-tab-line-mode -1))

(use-package term
  :custom
  (explicit-shell-file-name "/bin/bash")
  (explicit-bash-args '("--login"))
  :bind
  ([f12] . (lambda () (interactive)
             (term (concat
                    explicit-shell-file-name
                    " "
                    (mapconcat #'identity explicit-bash-args " ")))))
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
  (:map my-ctl-z-t-map
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

(use-package which-key
  :custom
  (which-key-idle-delay 1.9)
  :config
  (which-key-mode +1))

(use-package whitespace
  :custom
  (whitespace-line-column fill-column)
  (whitespace-style '(face lines-tail trailing))
  :hook
  (before-save . whitespace-cleanup)
  ((conf-mode prog-mode text-mode) . whitespace-mode)
  :bind
  (:map my-ctl-z-t-map
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
  (:map my-ctl-z-t-map
        ("k" .  keycast-mode-line-mode)))

(use-package magit
  :ensure t
  :custom
  (vc-follow-symlinks t)
  :bind
  (:map my-ctl-z-g-map
        ("g" . magit-status)
        ("j" . magit-dispatch)
        ("J" . magit-file-dispatch)
        ("r" . vc-refresh-state)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(provide 'my-packages)
