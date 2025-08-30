;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(use-package startup
  :custom
  (inhibit-startup-buffer-menu t)
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-startup-screen t)
  (inhibit-x-resources t)
  (initial-buffer-choice nil)
  (initial-major-mode 'org-mode)
  (initial-scratch-message nil)
  :hook
  (after-init . (lambda ()
                  (setopt file-name-handler-alist my-file-name-handlers
                          gc-cons-threshold (* 48 1024 1024)
                          gc-cons-percentage 0.2)

                  (makunbound 'my-file-name-handlers)

                  (garbage-collect)))

  (window-setup . toggle-frame-maximized)
  :config
  (run-with-idle-timer 15 t #'garbage-collect))

(use-package package
  :custom
  (package-archives
   '(("gnu"    . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("stable" . "https://stable.melpa.org/packages/")
     ("melpa"  . "https://melpa.org/packages/")))

  (package-archive-priorities
   '(("gnu"    . 3)
     ("nongnu" . 2)
     ("stable" . 1)
     ("melpa"  . 0))))

(use-package my-defaults)
(use-package my-platforms)

(use-package my-theme)
(use-package my-keybinds)
(use-package my-surround)
(use-package my-evil)
(use-package my-mode-line)

(use-package my-devel)
(use-package my-dired)
(use-package my-org)
(use-package my-vc)
(use-package my-window)

(use-package my-packages)

(provide 'init)
;;; init.el ends here
