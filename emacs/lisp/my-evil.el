;;; my-evil.el -*- lexical-binding: t; -*-

;; evil
(use-package evil
  :ensure t
  :preface
  (defun my-evil--propertize-state-tags ()
    (modus-themes-with-colors
      (let ((defaults `(:foreground ,white :weight bold)))
        (mapc (lambda (elt)
                (let ((tag-name (pop elt))
                      (tag-value (pop elt))
                      (tag-color (pop elt)))
                  (set-default tag-name
                               (propertize
                                tag-value
                                'face `(,@defaults :background ,tag-color)))))
              (list
               ;;   name                      value   color
               `(evil-normal-state-tag        " N "   ,blue)
               `(evil-insert-state-tag        " I "   ,green)
               `(evil-visual-char-tag         " V "   ,cyan)
               `(evil-visual-line-tag         " V "   ,cyan)
               `(evil-visual-block-tag        " V "   ,cyan)
               `(evil-visual-screen-line-tag  " S "   ,cyan)
               `(evil-operator-state-tag      " O "   ,yellow)
               `(evil-replace-state-tag       " R "   ,red)
               `(evil-motion-state-tag        " M "   ,black)
               `(evil-emacs-state-tag         " E "   ,magenta))))))
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
  (setopt evil-toggle-key "C-z C-z")
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
  (:map my-evil-leader-motion-state-map
        ("y" . my-evil-yank-to-clipboard))
  (:map my-evil-leader-normal-state-map
        ("p" . my-evil-paste-after-from-clipboard)
        ("P" . my-evil-paste-before-from-clipboard)
        ("Y" . my-evil-yank-eol-to-clipboard))
  (:map evil-insert-state-map
        ("C-x C-n" . evil-complete-next-line)
        ("C-x C-p" . evil-complete-previous-line))
  (:map evil-motion-state-map
        ("gc" . my-evil-commentary)
        ("gl" . my-evil-align-simple)
        ("gL" . my-evil-align-complex))
  (:map evil-normal-state-map
        ("] SPC" . (lambda (number-of-lines)
                     (interactive "p")
                     (dotimes (_ number-of-lines)
                       (evil-insert-newline-below)
                       (evil-previous-visual-line))))
        ("[ SPC" . (lambda (number-of-lines)
                     (interactive "p")
                     (dotimes (_ number-of-lines)
                       (evil-insert-newline-above)
                       (evil-next-visual-line)))))
  :init
  (my-evil--propertize-state-tags)
  :hook
  (evil-after-load
   . (lambda ()
       (evil-set-leader '(normal motion) (kbd "SPC"))
       (evil-set-leader '(normal motion) (kbd ",") t)

       (evil-define-command my-evil-paste-after-from-clipboard (count)
         :suppress-operator t
         (interactive "*P")
         (with-temp-buffer
           (my-paste-from-clipboard)
           (evil-set-register ?\" (buffer-string)))
         (evil-paste-after count))

       (evil-define-command my-evil-paste-before-from-clipboard (count)
         :suppress-operator t
         (interactive "*P")
         (with-temp-buffer
           (my-paste-from-clipboard)
           (evil-set-register ?\" (buffer-string)))
         (evil-paste-before count))

       (evil-define-operator my-evil-align-simple (beg end)
         :move-point nil
         :repeat t
         (interactive "<r>")
         (evil-with-active-region beg end
           (call-interactively #'my-align-simple)))

       (evil-define-operator my-evil-align-complex (beg end)
         :move-point nil
         :repeat t
         (interactive "<r>")
         (evil-with-active-region beg end
           (call-interactively #'my-align-complex)))

       (evil-define-operator my-evil-commentary (beg end)
         :move-point nil
         :repeat t
         (interactive "<r>")
         (comment-or-uncomment-region beg end))

       (evil-define-operator my-evil-yank-to-clipboard (beg end)
         :move-point nil
         :repeat nil
         (interactive "<r>")
         (evil-yank beg end)
         (my-copy-to-clipboard beg end))

       (evil-define-command my-evil-yank-eol-to-clipboard ()
         :suppress-operator t
         (interactive)
         (let ((beg (point))
               (end (pos-eol)))
           (evil-yank beg end)
           (my-copy-to-clipboard beg end)))))
  :config
  (evil-mode +1))

;; evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode +1))

(provide 'my-evil)
