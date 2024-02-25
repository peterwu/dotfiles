;;; my-evil.el -*- lexical-binding: t; -*-

;; evil
(use-package evil
  :ensure t
  :preface
  (defmacro my-evil-paste-from-clipboard (position)
    (let ((my-evil-paste-command (intern (concat "my-evil-paste-" (symbol-name position) "-from-clipboard")))
          (evil-paste-command (intern (concat "evil-paste-" (symbol-name position)))))
      `(evil-define-command ,my-evil-paste-command (count &optional register yank-handler)
         :suppress-operator t
         (interactive "*P<x>")
         (with-temp-buffer
           (call-process "wl-paste" nil t nil)
           (evil-set-register ?\" (buffer-string)))
         (,evil-paste-command count ?\" yank-handler))))

  ;; functions
  (defun my-propertize-evil-state-tags ()
    (let ((white "#FFFFFF"))
      (setq evil-normal-state-tag
            (propertize " N "
                        'face `(:foreground ,white :background "dark blue" :weight bold)))

      (setq evil-insert-state-tag
            (propertize " I "
                        'face `(:foreground ,white :background "dark green" :weight bold)))

      (setq evil-visual-char-tag
            (propertize " V "
                        'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq evil-visual-line-tag
            (propertize " L "
                        'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq evil-visual-screen-line-tag
            (propertize " S "
                        'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq evil-visual-block-tag
            (propertize " B "
                        'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq evil-operator-state-tag
            (propertize " O "
                        'face `(:foreground ,white :background "dark orange" :weight bold)))

      (setq evil-replace-state-tag
            (propertize " R "
                        'face `(:foreground ,white :background "dark red" :weight bold)))

      (setq evil-motion-state-tag
            (propertize " M "
                        'face `(:foreground ,white :background "black" :weight bold)))

      (setq evil-emacs-state-tag
            (propertize " E "
                        'face `(:foreground ,white :background "dark magenta" :weight bold)))))

  (defun my-smart-copy-to-clipboard ()
    (if (display-graphic-p)
        (evil-set-register ?+ (evil-get-register ?\"))
      (with-temp-buffer
        (insert (evil-get-register ?\"))
        (call-process-region (point-min) (point-max) "wl-copy" nil 0 nil))))

  :custom
  (evil-default-state 'emacs)
  (evil-disable-insert-state-bindings t)
  (evil-echo-state nil)
  (evil-mode-line-format nil)
  (evil-respect-visual-line-mode nil)
  (evil-undo-system 'undo-redo)
  (evil-want-C-i-jump nil)
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  :bind
  (:map evil-motion-state-map
        :prefix "<SPC>" :prefix-map my-evil-leader-motion-state-map)
  (:map evil-normal-state-map
        :prefix "<SPC>" :prefix-map my-evil-leader-normal-state-map)

  :bind
  (:map my-evil-leader-normal-state-map
        :prefix "g" :prefix-map my-evil-magit-map)
  :bind-keymap
  ("C-c g" . my-evil-magit-map)

  :bind
  (:map my-evil-leader-normal-state-map
        :prefix "j" :prefix-map my-evil-jump-map)
  :bind-keymap
  ("C-c j" . my-evil-jump-map)

  :bind
  (:map my-evil-leader-normal-state-map
        :prefix "o" :prefix-map my-evil-org-map)
  :bind-keymap
  ("C-c o" . my-evil-org-map)

  :bind
  (:map my-evil-leader-normal-state-map
        :prefix "t" :prefix-map my-evil-toggle-map)
  :bind-keymap
  ("C-c t" . my-evil-toggle-map)

  :bind
  (:map my-evil-leader-motion-state-map
        ("y" . my-evil-yank-to-clipboard)
        ("Y" . my-evil-yank-line-to-clipboard))

  (:map my-evil-leader-normal-state-map
        ("p" . my-evil-paste-after-from-clipboard)
        ("P" . my-evil-paste-before-from-clipboard)
        ("z" . text-scale-adjust))

  (:map evil-insert-state-map
        ("C-x C-n" . evil-complete-next-line)
        ("C-x C-p" . evil-complete-previous-line))

  :init
  (my-propertize-evil-state-tags)
  :hook
  (evil-after-load . (lambda ()
                       (evil-define-operator my-evil-yank-to-clipboard (beg end type register yank-handler)
                         :move-point nil
                         :repeat nil
                         (interactive "<R><x><y>")
                         (evil-yank beg end type ?\" yank-handler)
                         (my-smart-copy-to-clipboard))

                       (evil-define-operator my-evil-yank-line-to-clipboard (beg end type register)
                         :motion evil-line-or-visual-line
                         :move-point nil
                         (interactive "<R><x>")
                         (evil-yank-line beg end type ?\")
                         (my-smart-copy-to-clipboard))

                       (my-evil-paste-from-clipboard before)
                       (my-evil-paste-from-clipboard after)))
  (evil-after-load . (lambda ()
                       (fset 'evil-visual-update-x-selection 'ignore)))
  (evil-after-load . (lambda ()
                       (let ((modes '(conf-mode
                                      fundamental-mode
                                      prog-mode
                                      text-mode)))
                         (dolist (mode modes)
                           (evil-set-initial-state mode 'normal)))))
  :config
  (evil-mode +1))

;; evil-args
(use-package evil-args
  :ensure t
  :bind
  (:map evil-inner-text-objects-map
        ("a" . evil-inner-arg))
  (:map evil-outer-text-objects-map
        ("a" . evil-outer-arg))

  (:map evil-normal-state-map
        ("H" . evil-backward-arg)
        ("L" . evil-forward-arg)
        ("K" . evil-jump-out-args))

  (:map evil-motion-state-map
        ("H" . evil-backward-arg)
        ("L" . evil-forward-arg)))

;; evil-commentary
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode +1))

;; evil-exchange
(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

;; evil-goggles
(use-package evil-goggles
  :ensure t
  :preface
  (defun my-add-evil-commands-to-goggles ()
    (let ((commands (list
                     '(my-evil-yank-to-clipboard
                       :face evil-goggles-yank-face
                       :switch evil-goggles-enable-yank
                       :advice evil-goggles--generic-async-advice)

                     '(my-evil-yank-line-to-clipboard
                       :face evil-goggles-yank-face
                       :switch evil-goggles-enable-yank
                       :advice evil-goggles--generic-async-advice)

                     '(my-evil-paste-before-from-clipboard
                       :face evil-goggles-paste-face
                       :switch evil-goggles-enable-paste
                       :advice evil-goggles--paste-advice :after t)

                     '(my-evil-paste-after-from-clipboard
                       :face evil-goggles-paste-face
                       :switch evil-goggles-enable-paste
                       :advice evil-goggles--paste-advice :after t))))
      (dolist (command commands)
        (add-to-list 'evil-goggles--commands command))))
  :custom
  (evil-goggles-async-duration 0.900)
  (evil-goggles-blocking-duration 0.100)
  (evil-goggles-pulse t)
  :config
  (my-add-evil-commands-to-goggles)
  (evil-goggles-mode +1))

;; evil-lion
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode +1))

;; evil-matchit
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode +1))

;; evil-numbers
(use-package evil-numbers
  :ensure t
  :after evil
  :bind
  (:map my-evil-leader-nmap
        ("C-a" . evil-numbers/inc-at-pt)
        ("C-x" . evil-numbers/dec-at-pt)
        ("M-a" . evil-numbers/inc-at-pt-incremental)
        ("M-a" . evil-numbers/dec-at-pt-incremental)))

;; evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode +1))

(provide 'my-evil)
