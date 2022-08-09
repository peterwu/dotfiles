;;; init.el -*- lexical-binding: t; -*-

;; home baked package management
(defmacro with-package (package &rest body)
  (declare (indent 1))
  `(if (require ,package nil 'noerror)
       (progn ,@body)
     (progn (when (not (package-installed-p ,package))
              (unless package-archive-contents
                (package-refresh-contents))
              (package-install ,package))
            (if (require ,package nil 'noerror)
                (progn ,@body)
              (display-warning 'with-package
                               (format "Loading `%s' failed" ,package)
                               :warning)))))

(add-to-list 'load-path (locate-user-emacs-file "mine"))
(require 'my-package)

(require 'my-defaults)
(require 'my-msft)
(require 'my-mode-line)
(require 'my-theme)
(require 'my-dired)
(require 'my-evil)
(require 'my-gnus)
(require 'my-org)
(require 'my-window)

;; small packages
(with-package 'autorevert
  (setq auto-revert-verbose t)
  (global-auto-revert-mode +1))

(with-package 'avy
  (with-eval-after-load 'evil
    (setq avy-background t)

    ;; bind keys
    (define-key isearch-mode-map (kbd "C-'") #'avy-isearch)

    (define-key my-evil-jump-map (kbd "f") #'avy-goto-char)
    (define-key my-evil-jump-map (kbd "r") #'avy-resume)
    (define-key my-evil-jump-map (kbd "s") #'avy-goto-char-2)
    (define-key my-evil-jump-map (kbd "j") #'avy-goto-char-timer)
    (define-key my-evil-jump-map (kbd "w") #'avy-goto-word-1)
    (define-key my-evil-jump-map (kbd "W") #'avy-goto-word-0)))

(with-package 'battery
  (with-eval-after-load 'evil
    (setq battery-load-low 20)
    (setq battery-load-critical 10)
    (setq battery-mode-line-format "[%b%p%%]")
    (setq battery-mode-line-limit 95)
    (setq battery-update-interval 180)

    ;; bind keys
    (define-key my-evil-toggle-map (kbd "b") #'display-battery-mode)

    (display-battery-mode -1)))

(when (file-executable-p "/usr/bin/clang-format")
  (with-package 'clang-format
    (defvar my-clang-format-style "file")
    (defvar my-clang-format-fallback-style
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

    (defun my-clang-format-buffer-on-save ()
      (add-hook 'before-save-hook
                (lambda ()
                  (if (locate-dominating-file "." ".clang-format")
                      (clang-format-buffer my-clang-format-style)
                    (clang-format-buffer my-clang-format-fallback-style))
                  nil)
                nil
                t))

    (add-hook 'c-mode-hook #'my-clang-format-buffer-on-save)
    (add-hook 'c++-mode-hook #'my-clang-format-buffer-on-save)))

(with-package 'corfu
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-scroll-margin 5)

  (global-corfu-mode +1))

(with-package 'display-line-numbers
  (setq display-line-numbers-type 'relative)

  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  (with-eval-after-load 'evil
    (define-key my-evil-toggle-map (kbd "n") #'display-line-numbers-mode)))

(with-package 'ediff
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(with-package 'eglot
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure))

(with-package 'eldoc
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

(with-package 'electric
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars
        '(9
          10
          32))

  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-replace-double t)
  (setq electric-quote-string nil)

  (electric-indent-mode +1)
  (electric-pair-mode +1)
  (electric-quote-mode +1))

(with-package 'erc
  (setq erc-nick "ixlxi")
  (setq erc-user-full-name "iExcel")
  (setq erc-server "irc.libera.chat")
  (setq erc-port "6697"))

(with-package 'eshell
  (defun my-eshell-prompt-function ()
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
     " "))

  (defun my-turn-off-hl-line-in-eshell ()
    (setq-local global-hl-line-mode nil))


  (global-set-key (kbd "<f12>") #'eshell)

  (add-hook 'eshell-mode-hook #'my-turn-off-hl-line-in-eshell)

  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'my-eshell-prompt-function))


(with-package 'flymake
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-wrap-around nil)

  (require 'flymake)
  (define-key flymake-mode-map (kbd "C-c ! s") #'flymake-start)
  (define-key flymake-mode-map (kbd "C-c ! d") #'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error)

  (autoload-do-load 'flymake-mode))

(unless (eq system-type 'windows-nt)
  (with-package 'ispell
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US")

    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US")))

(unless (eq system-type 'windows-nt)
  (with-package 'flyspell
    (with-eval-after-load 'ispell
      (setq flyspell-issue-message-flag nil)
      (setq flyspell-issue-welcome-flag nil))))

(with-package 'gdb-mi
  (setq gdb-many-windows t)
  (setq gdb-restore-window-configuration-after-quit t)
  (setq gdb-show-main t))

(with-package 'ibuffer
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-expert t)
  (setq ibuffer-formats
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
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-old-time 48)
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-use-other-window nil)

  (add-hook 'ibuffer-mode-hook #'hl-line-mode))

(with-package 'icomplete
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)
  (setq icomplete-separator (propertize " · " 'face 'shadow))
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-tidy-shadowed-file-names t)
  (setq icomplete-with-completion-tables t)

  (define-key icomplete-minibuffer-map (kbd "<left>") #'icomplete-backward-completions)
  (define-key icomplete-minibuffer-map (kbd "<right>") #'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "<up>") #'icomplete-backward-completions)
  (define-key icomplete-minibuffer-map (kbd "<down>") #'icomplete-forward-completions)

  (fido-vertical-mode +1))

(with-package 'ido
  (defun my-customize-ido-decorations ()
    (setf (nth 0 ido-decorations) "\n")
    (setf (nth 1 ido-decorations) "")
    (setf (nth 2 ido-decorations)
          (propertize "\n" 'face 'shadow))
    (setf (nth 3 ido-decorations)
          (propertize
           (concat "\n" (if (char-displayable-p ?…) "…" "..."))
           'face 'shadow))
    (setf (nth 4 ido-decorations)
          (propertize "\n" 'face 'shadow))
    (setf (nth 5 ido-decorations)
          (propertize "" 'face 'shadow)))

  (setq ido-default-buffer-method 'selected-window)
  (setq ido-default-file-method 'selected-window)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-max-prospects 8)
  (setq ido-save-directory-list-file
        (expand-file-name "cache/ido.last" user-emacs-directory))
  (setq ido-show-dot-for-dired t)
  (setq ido-use-virtual-buffers t)

  (add-hook 'ido-minibuffer-setup-hook #'my-customize-ido-decorations)
  (add-hook 'ido-setup-hook (lambda ()
                              (define-key ido-completion-map (kbd "<left>") #'ido-prev-match)
                              (define-key ido-completion-map (kbd "<right>") #'ido-next-match)
                              (define-key ido-completion-map (kbd "<up>") #'ido-prev-match)
                              (define-key ido-completion-map (kbd "<down>") #'ido-next-match)))

  (ido-mode +1))


(with-package 'imenu
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-level-separator "/")
  (setq imenu-max-item-length 100)
  (setq imenu-space-replacement " ")
  (setq imenu-use-markers t)
  (setq imenu-use-popup-menu nil)

  (global-set-key (kbd "M-i") #'imenu))

(with-package 'isearch
  (setq isearch-allow-scroll 'unlimited)
  (setq isearch-lax-whitespace t)
  (setq isearch-lazy-count t)
  (setq isearch-lazy-highlight t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-yank-on-move 'shift)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)

  (define-key isearch-mode-map (kbd "C-g") #'isearch-cancel)
  (define-key isearch-mode-map (kbd "M-/") #'isearch-complete))

(with-package 'keycast
  (with-eval-after-load 'evil
    (setq keycast-mode-line-format "%k%c%R")
    (setq keycast-mode-line-insert-after 'wy:mode-line-centre-placeholder)
    (setq keycast-mode-line-remove-tail-elements nil)

    (define-key my-evil-toggle-map (kbd "k") #'keycast-mode)))

(with-package 'magit
  (global-set-key (kbd "C-x g") #'magit-status)

  (define-key my-evil-magit-map (kbd "g") #'magit-status)
  (define-key my-evil-magit-map (kbd "j") #'magit-dispatch)
  (define-key my-evil-magit-map (kbd "J") #'magit-file-dispatch)
  (define-key my-evil-magit-map (kbd "r") #'vc-refresh-state))

(with-package 'man
  (setq Man-notify-method 'bully)

  (define-key Man-mode-map (kbd "Q") (lambda () (interactive)
                                       (delete-other-frames)
                                       (delete-frame))))

(with-package 'minions
  (setq minions-mode-line-delimiters '("" . ""))
  (setq minions-mode-line-face 'mode-line-emphasis)
  (setq minions-mode-line-lighter (if (char-displayable-p ?…) "…" "...")))

(when (display-graphic-p)
  (with-package 'mouse
    (setq make-pointer-invisible t)
    (setq mouse-drag-copy-region nil)
    (setq mouse-wheel-follow-mouse t)
    (setq mouse-wheel-progressive-speed t)
    (setq mouse-wheel-scroll-amount
          '(1
            ((shift) . 5)
            ((meta) . 0.5)
            ((control) . text-scale)))

    (mouse-wheel-mode +1)))

(with-package 'paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)

  (show-paren-mode +1))

(with-package 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user)

  (autoload-do-load 'proced))

(with-package 'project
  (setq project-list-file (expand-file-name "cache/projects" user-emacs-directory))
  (setq project-switch-commands
        '((?f "File" project-find-file)
          (?g "Grep" project-find-regexp)
          (?D "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?r "Query replace" project-query-replace-regexp)
          (?m "Magit" magit-project-status)
          (?e "Eshell" project-eshell))))

(with-package 'rainbow-mode
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (add-hook 'prog-mode-hook #'rainbow-mode))

(with-package 'rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(with-package 'recentf
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (setq recentf-max-saved-items 200)
  (setq recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))

  (recentf-mode +1))

(with-package 'savehist
  (setq history-delete-duplicates t)
  (setq history-length 1000)
  (setq savehist-file (expand-file-name "cache/savehist" user-emacs-directory))
  (setq savehist-save-minibuffer-history t)

  (savehist-mode +1))

(with-package 'saveplace
  (setq save-place-file (expand-file-name "cache/places" user-emacs-directory))
  (setq save-place-forget-unreadable-files t)

  (save-place-mode +1))

(with-package 'tab-bar
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode -1)
  (tab-bar-history-mode -1))

(with-package 'tab-line
  (setq tab-line-new-button-show nil)
  (setq tab-line-close-button-show nil)
  (setq tab-line-exclude-modes '(completion-list-mode
                                 dired-sidebar-mode
                                 help-mode))

  (define-key my-evil-toggle-map (kbd "t") #'global-tab-line-mode )

  (global-tab-line-mode -1))

(with-package 'time
  (setq display-time-default-load-average nil)
  (setq display-time-format "[%H:%M]")
  (setq display-time-interval 60)

  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "New York")
          ("Europe/Brussels" "Brussels")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Tokyo" "Tokyo")))

  (setq world-clock-buffer-name "*world-clock*")
  (setq world-clock-list t)
  (setq world-clock-timer-enable t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-timer-second 60)

  (define-key my-evil-toggle-map (kbd "c") #'display-time-mode)
  (define-key my-evil-toggle-map (kbd "g") #'world-clock)

  (autoload-do-load 'world-clock)

  (display-time-mode -1))

(with-package 'tooltip
  (setq tooltip-delay 0.5)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)

  (tooltip-mode +1))

(with-package 'tramp
  (setq tramp-default-method "sshx")
  (setq tramp-persistency-file-name
        (expand-file-name "cache/tramp" user-emacs-directory)))

(with-package 'uniquify
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t))

(with-package 'which-key
  (which-key-mode +1))

(with-package 'whitespace
  (add-hook 'before-save-hook #'whitespace-cleanup)

  (define-key my-evil-toggle-map (kbd "w") #'whitespace-mode))

(unless (display-graphic-p)
  (with-package 'xt-mouse
    (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") #'scroll-up-line)

    (xterm-mouse-mode +1)))

(provide 'init)
;;; init.el ends here
