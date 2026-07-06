;;; init.el -*- lexical-binding: t; -*-

(use-package startup
  :no-require t
  :custom
  (inhibit-startup-buffer-menu t)
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-startup-screen t)
  (inhibit-x-resources t)
  (initial-buffer-choice nil)
  (initial-major-mode 'org-mode)
  (initial-scratch-message nil)
  :init
  (add-to-list 'load-path (locate-user-emacs-file "lisp"))

  (when (display-graphic-p)
    (let* ((pixel-height (display-pixel-height))
           (mm-height (display-mm-height))
           (dpi (if (and mm-height (> mm-height 0))
                    (/ (* pixel-height 25.4) mm-height)
                  96.0))
           (target-size 130)
           (modifier (cond ((> dpi 150) 0.85)
                           ((< dpi 90)  1.15)
                           (t 1.25)))
           (calculated-height (truncate (* target-size modifier)))
           (face-settings '((default        . "SF Mono")
                            (fixed-pitch    . "SF Mono")
                            (variable-pitch . "SF Pro"))))
      (dolist (setting face-settings)
        (let ((face (car setting))
              (family (cdr setting)))
          (set-face-attribute face nil
                              :family family
                              :weight 'normal
                              :height calculated-height)))))
  :bind
  (:map global-map
        ("C-z" . nil)

        ("C-x C-c" . nil)
        ("C-x C-c C-c" . save-buffers-kill-terminal))
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
     ("melpa"  . "https://snapshots.melpa.org/packages/")))

  (package-archive-priorities
   '(("gnu"    . 3)
     ("nongnu" . 2)
     ("melpa"  . 1))))

(use-package my-defaults)
(use-package my-platforms)

(use-package my-theme)
(use-package my-keymaps)
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
