;;; my-evil.el -*- lexical-binding: t; -*-

;; evil
(use-package evil
  :ensure t
  :pin melpa
  :after my-surround
  :preface
  (defun my-evil-propertize-state-tags ()
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

  (defun my-evil-surround-call-with-repeat (callback)
    "Record keystrokes to repeat surround-region operator and it's motion.
This is necessary because `evil-yank' operator is not repeatable (:repeat nil)"
    (evil-repeat-start)
    (evil-repeat-record "y")
    (evil-repeat-record (this-command-keys))

    (setq this-command callback)
    (call-interactively callback)
    (evil-repeat-keystrokes 'post)
    (evil-repeat-stop))
  :init
  (setopt evil-default-state 'emacs)
  (setopt evil-emacs-state-modes nil)
  (setopt evil-insert-state-modes nil)
  (setopt evil-motion-state-modes nil)
  (setopt evil-normal-state-modes nil)

  (setopt evil-buffer-regexps
          '(("^ \\*load\\*" . nil)
            ("\\`\\*scratch\\*\\'" . normal)))
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
        ("g c" . my-evil-commentary)
        ("g l" . my-evil-align-left)
        ("g L" . my-evil-align-right)

        ("M-y" . my-evil-yank-to-clipboard))
  (:map evil-normal-state-map
        ("] b" . next-buffer)
        ("[ b" . previous-buffer)

        ("] SPC" . (lambda (number-of-lines)
                     (interactive "p")
                     (dotimes (_ number-of-lines)
                       (evil-insert-newline-below)
                       (evil-previous-visual-line))))
        ("[ SPC" . (lambda (number-of-lines)
                     (interactive "p")
                     (dotimes (_ number-of-lines)
                       (evil-insert-newline-above)
                       (evil-next-visual-line))))

        ("M-p" . my-evil-paste-after-from-clipboard)
        ("M-P" . my-evil-paste-before-from-clipboard)
        ("M-Y" . my-evil-yank-eol-to-clipboard))
  (:map evil-operator-state-map
        ("s" . my-evil-surround-edit))
  (:map evil-visual-state-map
        ("S" . my-surround-region))
  :hook
  (change-major-mode . evil-change-to-initial-state)

  (after-change-major-mode
   . (lambda ()
       (unless (or (minibufferp)
                   buffer-read-only)
         (evil-normal-state))))
  :config
  (evil-set-leader '(normal motion) (kbd "SPC"))
  (evil-set-leader '(normal motion) (kbd ",") t)
  :config
  (evil-define-command my-evil-paste-after-from-clipboard (count)
    :suppress-operator t
    (interactive "*P")
    (with-temp-buffer
      (clipboard-yank)
      (evil-set-register ?\" (buffer-string)))
    (evil-paste-after count))

  (evil-define-command my-evil-paste-before-from-clipboard (count)
    :suppress-operator t
    (interactive "*P")
    (with-temp-buffer
      (clipboard-yank)
      (evil-set-register ?\" (buffer-string)))
    (evil-paste-before count))

  (evil-define-operator my-evil-align-left (beg end)
    :move-point nil
    :repeat t
    (interactive "<r>")
    (evil-with-active-region beg end
      (call-interactively #'my-align-left)))

  (evil-define-operator my-evil-align-right (beg end)
    :move-point nil
    :repeat t
    (interactive "<r>")
    (evil-with-active-region beg end
      (call-interactively #'my-align-right)))

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
    (clipboard-kill-ring-save beg end))

  (evil-define-command my-evil-yank-eol-to-clipboard ()
    :suppress-operator t
    (interactive)
    (let ((beg (point))
          (end (pos-eol)))
      (evil-yank beg end)
      (clipboard-kill-ring-save beg end)))

  (evil-define-command my-evil-surround-edit (operation)
    (interactive
     (list (assoc-default evil-this-operator
                          '((evil-change . change)
                            (evil-delete . delete)))))

    (setq evil-inhibit-operator t)
    (cond
     ((eq operation 'change)
      (call-interactively 'my-surround-change))
     ((eq operation 'delete)
      (call-interactively 'my-surround-delete))
     (t
      (my-evil-surround-call-with-repeat 'my-evil-surround-region)))

    ;; Return an empty range so evil-motion-range doesn't try to guess
    (let ((p (point))) (list p p 'exclusive)))

  (evil-define-operator my-evil-surround-region (beg end)
    (interactive "<r>")
    (let ((char (read-char)))
      (my-surround-region beg end char)))
  :config
  (my-evil-propertize-state-tags)
  (evil-mode +1))

(provide 'my-evil)
