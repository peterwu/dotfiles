;;;; init.el --- Initialization file for Emacs

(require 'package)
(require 'org)

;;; Tell custom to put its crap somewhere else, but load it early
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "mine" user-emacs-directory))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package auto-package-update
             :init
  (setq auto-package-update-last-update-day-filename (expand-file-name ".cache/last-package-update-day" user-emacs-directory))
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-interval 30)
  (auto-package-update-maybe))

(defvar better-gc-cons-threshold (* 256 1024 1024) ; 256MB
  "The default value to use for `gc-cons-threshold'.
  If you experience freezing, decrease this. If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold better-gc-cons-threshold)
	    (setq file-name-handler-alist file-name-handler-alist-original)
	    (makunbound 'file-name-handler-alist-original)))

(org-babel-load-file
 (expand-file-name "config.org"
		   user-emacs-directory))
