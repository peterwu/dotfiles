;;; my-evil.el -*- lexical-binding: t; -*-

;; evil
(defmacro my-evil-paste-from-clipboard (position)
  (let ((my-evil-paste-command (intern (concat "my-evil-paste-" (symbol-name position) "-from-clipboard")))
        (evil-paste-command (intern (concat "evil-paste-" (symbol-name position)))))
    `(evil-define-command ,my-evil-paste-command (count &optional register yank-handler)
       :suppress-operator t
       (interactive "*P<x>")
       (if (display-graphic-p)
           (evil-set-register ?\" (evil-get-register ?+))
         (with-temp-buffer
           (call-process "xsel" nil t nil "--output" "--clipboard")
           (evil-set-register ?\" (buffer-string))))
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
          (propertize " Vl "
                      'face `(:foreground ,white :background "dark cyan" :weight bold)))

    (setq evil-visual-screen-line-tag
          (propertize " Vs "
                      'face `(:foreground ,white :background "dark cyan" :weight bold)))

    (setq evil-visual-block-tag
          (propertize " Vb "
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

(defun my-define-evil-commands ()
  (defun my-smart-copy-to-clipboard ()
    (if (display-graphic-p)
        (evil-set-register ?+ (evil-get-register ?\"))
      (with-temp-buffer
        (insert (evil-get-register ?\"))
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil  "--input" "--clipboard"))))

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

  ;; my-evil-paste-before-from-clipboard
  (my-evil-paste-from-clipboard before)
  ;; my-evil-paste-after-from-clipboard
  (my-evil-paste-from-clipboard after))

(defun my-ignore-some-evil-functions ()
  (fset 'evil-visual-update-x-selection 'ignore))

(setq evil-echo-state nil)
(setq evil-mode-line-format nil)
(setq evil-respect-visual-line-mode nil)
(setq evil-undo-system 'undo-redo)
(setq evil-want-C-i-jump nil)
(setq evil-want-Y-yank-to-eol t)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(with-package 'evil
  (my-evil-paste-from-clipboard before)
  (my-evil-paste-from-clipboard after)

  ;; define prefix keymaps
  (define-prefix-command 'my-evil-leader-mmap)
  (define-key evil-motion-state-map (kbd "SPC") 'my-evil-leader-mmap)

  (define-prefix-command 'my-evil-leader-nmap)
  (define-key evil-normal-state-map (kbd "SPC") 'my-evil-leader-nmap)

  (define-prefix-command 'my-evil-jump-map)
  (define-key my-evil-leader-nmap (kbd "j") 'my-evil-jump-map)
  (global-set-key (kbd "C-c j") my-evil-jump-map)

  (define-prefix-command 'my-evil-magit-map)
  (define-key my-evil-leader-nmap (kbd "g") 'my-evil-magit-map)
  (global-set-key (kbd "C-c g") my-evil-magit-map)

  (define-prefix-command 'my-evil-toggle-map)
  (define-key my-evil-leader-nmap (kbd "t") 'my-evil-toggle-map)
  (global-set-key (kbd "C-c t") my-evil-toggle-map)

  ;; bing keys
  (define-key my-evil-leader-mmap (kbd "y") #'my-evil-yank-to-clipboard)
  (define-key my-evil-leader-mmap (kbd "Y") #'my-evil-yank-line-to-clipboard)

  (define-key my-evil-leader-nmap (kbd "p") #'my-evil-paste-after-from-clipboard)
  (define-key my-evil-leader-nmap (kbd "P") #'my-evil-paste-before-from-clipboard)
  (define-key my-evil-leader-nmap (kbd "z") #'text-scale-adjust)

  ;; initialize
  (my-define-evil-commands)
  (my-ignore-some-evil-functions)
  (my-propertize-evil-state-tags)

  (evil-mode +1))

;; evil-collection
(with-package 'evil-collection
  (with-eval-after-load 'evil
    (setq evil-collection-setup-minibuffer t)
    (evil-collection-init)))

;; evil-args
(with-package 'evil-args
  (define-key evil-inner-text-objects-map (kbd "a") #'evil-inner-arg)
  (define-key evil-outer-text-objects-map (kbd "a") #'evil-outer-arg)

  (define-key evil-normal-state-map (kbd "H") #'evil-backward-arg)
  (define-key evil-normal-state-map (kbd "L") #'evil-forward-arg)
  (define-key evil-normal-state-map (kbd "K") #'evil-jump-out-args)

  (define-key evil-motion-state-map (kbd "H") #'evil-backward-arg)
  (define-key evil-motion-state-map (kbd "L") #'evil-forward-arg))

;; evil-commentary
(with-package 'evil-commentary
  (evil-commentary-mode +1))

;; evil-exchange
(with-package 'evil-exchange
  (evil-exchange-install))

;; evil-goggles
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

(with-package 'evil-goggles
  (setq evil-goggles-async-duration 0.900)
  (setq evil-goggles-blocking-duration 0.100)
  (setq evil-goggles-pulse t)

  (my-add-evil-commands-to-goggles)

  (evil-goggles-mode +1))

;; evil-lion
(with-package 'evil-lion
  (evil-lion-mode +1))

;; evil-matchit
(with-package 'evil-matchit
  (global-evil-matchit-mode +1))

;; evil-numbers
(with-package 'evil-numbers
  (with-eval-after-load 'evil
    (define-key my-evil-leader-nmap (kbd "C-a") #'evil-numbers/inc-at-pt)
    (define-key my-evil-leader-nmap (kbd "C-x") #'evil-numbers/dec-at-pt)
    (define-key my-evil-leader-nmap (kbd "M-a") #'evil-numbers/inc-at-pt-incremental)
    (define-key my-evil-leader-nmap (kbd "M-a") #'evil-numbers/dec-at-pt-incremental)))

;; evil-surround
(with-package 'evil-surround
  (global-evil-surround-mode +1))

(provide 'my-evil)
