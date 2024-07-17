;;; my-org.el -*- lexical-binding: t; -*-

;; org
(use-package org
  :custom
  (org-ellipsis " ▾")
  (org-export-headline-levels 5)
  (org-export-with-tags nil)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-imenu-depth 7)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-odt-convert-process 'unoconv)
  (org-odt-preferred-output-format "docx")
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-startup-folded t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)

  (org-agenda-files (list "~/Documents/Org"))
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/Documents/Org/todo.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("n" "Notes" entry (file+headline "~/Documents/Org/notes.org" "Notes")
      "* Notes %?\n  %i\n  %a")
     ("j" "Journal" entry (file+olp+datetree "~/Documents/Org/journal.org")
      "* %?\nEntered on %U\n  %i\n  %a")))
  :bind
  (:map my-ctl-z-o-map
        ("a" . org-agenda)
        ("c" . org-capture))
  :hook
  (org-mode . (lambda ()
                (display-line-numbers-mode -1)
                (org-indent-mode +1)
                (variable-pitch-mode -1))))

(provide 'my-org)
