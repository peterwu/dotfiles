;;; my-vc.el -*- lexical-binding: t; -*-

(use-package diff-mode
  :defer t
  :custom
  (diff-default-read-only t)
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package log-view
  :defer t
  :bind
  (:map log-view-mode-map
        ("f" . nil)

        ("TAB" . log-view-toggle-entry-display)
        ("RET" . log-view-find-revision)

        ("F" . vc-pull)
        ("I" . vc-log-incoming)
        ("O" . vc-log-outgoing)
        ("P" . vc-push)
        ("s" . vc-log-search)))

(use-package project
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     (keyboard-quit "Quit"))
   (project-vc-extra-root-markers '(".project")))
  :bind
  (:map global-map
        ("C-x p DEL" . project-forget-project))
  (:map project-prefix-map
        ("q" . keyboard-quit)))

(use-package vc
  :custom
  (vc-find-revision-no-save t)
  (vc-follow-symlinks t)

  ;; git
  (vc-handled-backends '(Git))

  ;; 50/72 rule
  (vc-git-log-edit-summary-target-len 50)

  (vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (vc-git-log-switches '("--stat"))
  (vc-git-print-log-follow t)
  (vc-git-revision-complete-only-branches nil)
  (vc-git-root-log-format
   `("%d %h %ai %an: %s"
     ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
              "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
              "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
              "\\(?3:.*?\\):")
     ((1 'log-view-message)
      (2 'change-log-list nil lax)
      (3 'change-log-name)
      (4 'change-log-date))))
  :bind
  (:map global-map
        ("C-x v x" . nil)

        ("C-x v +" . vc-register)
        ("C-x v ." . vc-dir)
        ("C-x v =" . vc-ediff)
        ("C-x v B" . vc-annotate)
        ("C-x v F" . vc-pull)
        ("C-x v G" . vc-pull-and-push)
        ("C-x v d" . vc-diff)
        ("C-x v g" . vc-refresh-state)
        ("C-x v i" . vc-ignore)
        ("C-x v k" . vc-delete-file)
        ("C-x v s" . vc-log-search)
        ("C-x v t" . vc-create-tag))
  :config
  (advice-add #'vc-git-expanded-log-entry
              :filter-return (lambda (r)
                               (concat "\n" r))))

(use-package vc-dir
  :defer t
  :bind
  (:map vc-dir-mode-map
        ("+" . vc-register)
        ("=" . vc-ediff)
        ("F" . vc-pull)
        ("G" . vc-pull-and-push)
        ("O" . vc-log-outgoing)
        ("d" . vc-diff)
        ("i" . vc-dir-ignore)
        ("k" . vc-dir-delete-file)
        ("o" . vc-dir-find-file-other-window)
        ("t" . vc-create-tag)
        ("u" . vc-revert)))

(provide 'my-vc)
