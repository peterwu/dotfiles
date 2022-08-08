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
        '(("t" "Todo" entry (file+headline "~/Documents/Org/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Notes" entry (file+headline "~/Documents/Org/notes.org" "Notes")
           "* Notes %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/Documents/Org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))

  ;; hooks
  (add-hook 'org-mode-hook (lambda ()
                             (variable-pitch-mode -1)
                             (display-line-numbers-mode -1))))

;; org-superstar
(with-package 'org-superstar
  (setq org-superstar-headline-bullets-list '("⁖" "‣" "◉" "•" "▣" "⁕" "★" "✓"))
  (setq org-superstar-leading-bullet ?\s)
  (setq org-superstar-prettify-item-bullets t)

  ;; hooks
  (add-hook 'org-mode-hook #'org-superstar-mode))

(provide 'my-org)
