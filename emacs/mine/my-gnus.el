;;; my-gnus.el -*- lexical-binding: t; -*-

;; gnus
(use-package gnus
  :custom
  (user-full-name "Peter Wu")
  (user-mail-address "peterwu@hotmail.com")

  (gnus-home-directory (expand-file-name "gnus" user-emacs-directory))
  (gnus-startup-file (expand-file-name ".newsrc" gnus-home-directory))
  (gnus-directory (expand-file-name "news" gnus-home-directory))
  (message-directory (expand-file-name "mail" gnus-home-directory))

  (auth-sources `(,(expand-file-name ".authinfo" gnus-home-directory)))
  (mail-signature-file (expand-file-name ".signature" gnus-home-directory))
  (mail-user-agent 'gnus-user-agent)

  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a %d %b %Y at %R %z, %N wrote:")

  (mm-discouraged-alternatives '("text/html" "text/richtext"))

  (gnus-always-read-dribble-file nil)
  (gnus-asynchronous t)
  (gnus-blocked-images nil)
  (gnus-expert-user t)
  (gnus-interactive-exit nil)
  (gnus-novice-user nil)
  (gnus-show-threads t)
  (gnus-use-dribble-file nil)

  (gnus-select-method '(nnimap "hotmail"
                               (nnimap-address "outlook.office365.com")
                               (nnimap-server-port 993)
                               (nnimap-stream ssl)
                               (nnimap-authenticator login)))

  (gnus-secondary-select-methods
   '((nntp "news.gmane.io")
     (nntp "free.xsusenet.com")))

  (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "smtp.office365.com")
  (smtpmail-smtp-service 587)
  :hook
  (gnus-group-mode . gnus-topic-mode))

(provide 'my-gnus)
