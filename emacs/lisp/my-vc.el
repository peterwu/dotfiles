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
  ("C-x v +" . vc-register)
  ("C-x v ." . vc-dir-root)
  ("C-x v =" . vc-ediff)
  ("C-x v B" . vc-annotate)
  ("C-x v F" . vc-pull)
  ("C-x v G" . vc-pull-and-push)
  ("C-x v d" . vc-diff)
  ("C-x v g" . vc-refresh-state)
  ("C-x v i" . vc-ignore)
  ("C-x v s" . vc-log-search)
  ("C-x v t" . vc-create-tag)
  :config
  (advice-add #'vc-git-expanded-log-entry
              :filter-return (lambda (r)
                               (concat "\n" r))))

(provide 'my-vc)
