;;; my-gnus.el -*- lexical-binding: t; -*-

;; gnus
(use-package gnus
  :preface
  (defvar my-gnus-home-directory
    (expand-file-name "gnus" user-emacs-directory))

  (unless (file-directory-p my-gnus-home-directory)
    (make-directory my-gnus-home-directory))
  :custom
  (user-full-name "Peter Wu")
  (user-mail-address "peterwu@hotmail.com")

  (gnus-home-directory my-gnus-home-directory)
  (auth-sources `(,(expand-file-name ".authinfo" gnus-home-directory)))
  (gnus-startup-file (expand-file-name ".newsrc" gnus-home-directory))
  (mail-signature-file (expand-file-name ".signature" gnus-home-directory))

  (mail-user-agent 'gnus-user-agent)

  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a %d %b %Y at %R %z, %N wrote:")

  (mm-discouraged-alternatives '("text/html" "text/richtext"))

  (gnus-always-read-dribble-file nil)
  (gnus-blocked-images nil)
  (gnus-expert-user t)
  (gnus-interactive-exit nil)
  (gnus-novice-user nil)

  (gnus-read-newsrc-file nil)
  (gnus-save-killed-list nil)
  (gnus-save-newsrc-file nil)

  (gnus-show-threads t)
  (gnus-use-dribble-file nil)

  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods
   '((nntp "news.eternal-september.org")
     (nntp "news.gmane.io")))
  :hook
  (gnus-group-mode . gnus-topic-mode))

(provide 'my-gnus)
