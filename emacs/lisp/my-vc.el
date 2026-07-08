;;; my-vc.el -*- lexical-binding: t; -*-

(use-package diff-mode
  :custom
  (diff-default-read-only t)
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also))

(use-package ediff
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :hook
  (ediff-after-setup-windows
   . (lambda ()
       (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
         (with-selected-window (get-buffer-window buf)
           (tab-line-mode -1)))))

  (ediff-cleanup
   . (lambda ()
       (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
         (when (and buf
                    (buffer-live-p buf)
                    (string-match-p
                     "[^/]+\\.~[[:xdigit:]]+~\\'"
                     (buffer-name buf)))
           (kill-buffer buf)))))

  (ediff-quit . winner-undo))

(use-package log-view
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
  (project-list-file
   (expand-file-name "cache/projects.eld" user-emacs-directory))
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (ghostel-project "Ghostel")
     (ghostel-project-list-buffers "Ghostel buffers")
     (keyboard-quit "Quit")))
  (project-vc-extra-root-markers '(".project"))
  :bind
  (:map project-prefix-map
        ("DEL" . project-forget-project)
        ("t"   . ghostel-project)
        ("T"   . ghostel-project-list-buffers)
        ("q"   . keyboard-quit)))

(use-package vc
  :custom
  (vc-allow-rewriting-published-history 'ask)
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
  (:map vc-prefix-map
        ("F" . vc-pull))
  :config
  (advice-add #'vc-git-expanded-log-entry
              :filter-return (lambda (r)
                               (concat "\n" r))))

(provide 'my-vc)
