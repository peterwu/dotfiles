;;; init.el --- Initialization file for Emacs

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 256 1024 1024) ; 256MB
		  gc-cons-percentage 0.1
		  file-name-handler-alist my--file-name-handler-alist)))

;; Tell custom to put its crap somewhere else, but load it early
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "mine" user-emacs-directory))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package auto-package-update
  :init
  (setq auto-package-update-last-update-day-filename
	(expand-file-name ".cache/last-package-update-day" user-emacs-directory))
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-interval 13)
  (auto-package-update-maybe))

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
