;;; my-evil.el -*- lexical-binding: t; -*-

;; evil
(use-package evil
  :ensure t
  :preface
  (defvar my-cli-copy-command
    (cond ((eq system-type 'darwin) "pbcopy")
          ((eq system-type 'gnu/linux) "wl-copy")))

  (defvar my-cli-paste-command
    (cond ((eq system-type 'darwin) "pbpaste")
          ((eq system-type 'gnu/linux) "wl-paste")))

  (defmacro my-evil-paste-from-clipboard (position)
    (let ((my-evil-paste-command (intern (concat "my-evil-paste-" (symbol-name position) "-from-clipboard")))
          (evil-paste-command (intern (concat "evil-paste-" (symbol-name position))))
          (cli-paste-command (split-string my-cli-paste-command)))
      `(evil-define-command ,my-evil-paste-command (count &optional register yank-handler)
         :suppress-operator t
         (interactive "*P<x>")
         (with-temp-buffer
           (call-process ,(car cli-paste-command) nil t nil ,@(cdr cli-paste-command))
           (evil-set-register ?\" (buffer-string)))
         (,evil-paste-command count ?\" yank-handler))))

  (defmacro my-evil-copy-to-clipboard ()
    (let ((cli-copy-command (split-string my-cli-copy-command)))
      `(with-temp-buffer
         (insert (evil-get-register ?\"))
         (call-process-region (point-min) (point-max) ,(car cli-copy-command) nil 0 nil ,@(cdr cli-copy-command)))))

  (defun my-propertize-evil-state-tags ()
    (let ((white "#FFFFFF"))
      (setq-default evil-normal-state-tag
                    (propertize " N "
                                'face `(:foreground ,white :background "dark blue" :weight bold)))

      (setq-default evil-insert-state-tag
                    (propertize " I "
                                'face `(:foreground ,white :background "dark green" :weight bold)))

      (setq-default evil-visual-char-tag
                    (propertize " V "
                                'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq-default evil-visual-line-tag
                    (propertize " L "
                                'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq-default evil-visual-screen-line-tag
                    (propertize " S "
                                'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq-default evil-visual-block-tag
                    (propertize " B "
                                'face `(:foreground ,white :background "dark cyan" :weight bold)))

      (setq-default evil-operator-state-tag
                    (propertize " O "
                                'face `(:foreground ,white :background "dark orange" :weight bold)))

      (setq-default evil-replace-state-tag
                    (propertize " R "
                                'face `(:foreground ,white :background "dark red" :weight bold)))

      (setq-default evil-motion-state-tag
                    (propertize " M "
                                'face `(:foreground ,white :background "black" :weight bold)))

      (setq-default evil-emacs-state-tag
                    (propertize " E "
                                'face `(:foreground ,white :background "dark magenta" :weight bold)))))

  :init
  (setopt evil-default-state 'emacs)
  (setopt evil-emacs-state-modes nil)
  (setopt evil-insert-state-modes nil)
  (setopt evil-motion-state-modes nil)
  (setopt evil-normal-state-modes '(conf-mode
                                    fundamental-mode
                                    prog-mode
                                    text-mode))

  (setopt evil-disable-insert-state-bindings t)
  (setopt evil-echo-state nil)
  (setopt evil-mode-line-format '(after . nil))
  (setopt evil-respect-visual-line-mode nil)
  (setopt evil-undo-system 'undo-redo)
  (setopt evil-want-C-i-jump nil)
  (setopt evil-want-C-w-in-emacs-state t)
  (setopt evil-want-Y-yank-to-eol t)
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil)

  :bind
  (:map evil-motion-state-map
        :prefix "<leader>" :prefix-map my-evil-leader-motion-state-map)
  (:map evil-normal-state-map
        :prefix "<leader>" :prefix-map my-evil-leader-normal-state-map)

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
  (:map evil-normal-state-map
        ("] SPC" . (lambda (number-of-lines)
                     (interactive "P")
                     (dotimes (_ (or number-of-lines 1))
                       (evil-insert-newline-below)
                       (evil-previous-visual-line))))
        ("[ SPC" . (lambda (number-of-lines)
                     (interactive "P")
                     (dotimes (_ (or number-of-lines 1))
                       (evil-insert-newline-above)
                       (evil-next-visual-line)))))
  :init
  (my-propertize-evil-state-tags)
  :hook
  (evil-after-load . (lambda ()
                       (evil-set-leader '(normal motion) (kbd "SPC"))
                       (evil-set-leader '(normal motion) (kbd ",") t)

                       (evil-define-operator my-evil-yank-to-clipboard (beg end type register yank-handler)
                         :move-point nil
                         :repeat nil
                         (interactive "<R><x><y>")
                         (evil-yank beg end type ?\" yank-handler)
                         (my-evil-copy-to-clipboard))

                       (evil-define-operator my-evil-yank-line-to-clipboard (beg end type register)
                         :motion evil-line-or-visual-line
                         :move-point nil
                         (interactive "<R><x>")
                         (evil-yank-line beg end type ?\")
                         (my-evil-copy-to-clipboard))

                       (my-evil-paste-from-clipboard before)
                       (my-evil-paste-from-clipboard after)))
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
  (:map my-evil-leader-normal-state-map
        ("C-a" . evil-numbers/inc-at-pt)
        ("C-x" . evil-numbers/dec-at-pt)
        ("M-a" . evil-numbers/inc-at-pt-incremental)
        ("M-a" . evil-numbers/dec-at-pt-incremental)))

;; evil-surround
(use-package evil-surround
  :ensure t
  :pin melpa
  :config
  (global-evil-surround-mode +1))

(provide 'my-evil)
