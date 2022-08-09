;;; my-gnus.el -*- lexical-binding: t; -*-

;; gnus
(setq user-full-name "Peter Wu")
(setq user-mail-address "peterwu@hotmail.com")

(setq gnus-home-directory (expand-file-name "gnus" user-emacs-directory))
(setq gnus-startup-file (expand-file-name ".newsrc" gnus-home-directory))
(setq gnus-directory (expand-file-name "news" gnus-home-directory))
(setq message-directory (expand-file-name "mail" gnus-home-directory))

(setq auth-sources `(,(expand-file-name ".authinfo" gnus-home-directory)))
(setq mail-signature-file (expand-file-name ".signature" gnus-home-directory))
(setq mail-user-agent 'gnus-user-agent)

(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a %d %b %Y at %R %z, %N wrote:")

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-always-read-dribble-file nil)
(setq gnus-asynchronous t)
(setq gnus-blocked-images nil)
(setq gnus-expert-user t)
(setq gnus-interactive-exit nil)
(setq gnus-novice-user nil)
(setq gnus-show-threads t)
(setq gnus-use-dribble-file nil)

(setq gnus-select-method '(nnimap "hotmail"
                                  (nnimap-address "outlook.office365.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnimap-authenticator login)))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        (nntp "free.xsusenet.com")))

(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.office365.com")
(setq smtpmail-smtp-service 587)

;; transients
(with-eval-after-load 'transient
  (transient-define-prefix my-gnus-transient-gnus-group-list-groups
    "gnus-group listing groups"
    ["Listing groups"
     ("s" "List unread subscribed groups" gnus-group-list-groups)
     ("u" "List all groups" gnus-group-list-all-groups)])

  (transient-define-prefix my-gnus-transient-gnus-topic-commands
    "gnus-topic topic commands"
    ["Topic commands"
     ("m" "Move group to other topic" gnus-topic-move-group)
     ("h" "Hide topic" gnus-topic-hide-topic)
     ("s" "Show topic" gnus-topic-show-topic)])

  (transient-define-prefix my-gnus-transient-gnus-summary-mail-group-commands
    "gnus-summary mail group commands"
    ["Mail group commands"
     ("d" "Delete mail article" gnus-summary-delete-article)]))

;; setups
(with-eval-after-load 'evil
  (defun my-gnus-group-setup()
    (evil-define-key 'normal gnus-group-mode-map "A" 'my-gnus-transient-gnus-group-list-groups)

    (evil-define-key 'normal gnus-group-mode-map "l" 'gnus-group-list-groups)
    (evil-define-key 'normal gnus-group-mode-map "L" 'gnus-group-list-all-groups))

  (defun my-gnus-topic-setup ()
    (evil-define-key 'normal gnus-topic-mode-map "T" 'my-gnus-transient-gnus-topic-commands))

  (defun my-gnus-summary-setup()
    (evil-define-key 'normal gnus-summary-mode-map "B" 'my-gnus-transient-gnus-summary-mail-group-commands)

    (evil-define-key 'normal gnus-summary-mode-map "t" 'gnus-summary-toggle-header)))

(defun my-gnus-setup ()
  (my-gnus-group-setup)
  (my-gnus-topic-setup)
  (my-gnus-summary-setup))

(with-package 'gnus
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

  (my-gnus-setup))

(provide 'my-gnus)
