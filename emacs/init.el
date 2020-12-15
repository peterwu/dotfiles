;;; init.el --- Initialization file for Emacs

(require 'package)
(require 'org)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 8 1024 1024) ; 8MB
		  gc-cons-percentage 0.1
		  file-name-handler-alist my-file-name-handler-alist)))
(run-with-idle-timer 5 nil (lambda () (garbage-collect)))

;; Tell custom to put its crap somewhere else, but load it early
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
