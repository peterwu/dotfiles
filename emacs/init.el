;;; init.el --- Initialization file for Emacs

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 256 1024 1024) ; 256MB
		  gc-cons-percentage 0.1
		  file-name-handler-alist my-file-name-handler-alist)))

;; Tell custom to put its crap somewhere else, but load it early
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "mine" user-emacs-directory))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; osc52 support under supporting terminals and tmux
(use-package osc52
  :ensure nil
  :config
  (osc52-set-cut-function))

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
