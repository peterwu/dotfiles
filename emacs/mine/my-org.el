;;; my-org.el -*- lexical-binding: t; -*-

;; org
(with-package 'org
  (setq org-ellipsis " ▾")
  (setq org-export-headline-levels 5)
  (setq org-export-with-tags nil)
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-odt-convert-process 'unoconv)
  (setq org-odt-preferred-output-format "docx")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-folded t)
  (setq org-startup-with-inline-images t)
  (setq org-support-shift-select t)

  (setq org-agenda-files (list "~/Documents/Org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/Org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Notes" entry (file+headline "~/Documents/Org/notes.org" "Notes")
           "* Notes %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/Documents/Org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  ;; key binds
  (with-eval-after-load 'evil
    (define-key my-evil-org-map (kbd "a") #'org-agenda)
    (define-key my-evil-org-map (kbd "c") #'org-capture))

  ;; hooks
  (add-hook 'org-mode-hook (lambda ()
                             (variable-pitch-mode -1)
                             (display-line-numbers-mode -1)
                             (org-indent-mode +1))))

(provide 'my-org)
