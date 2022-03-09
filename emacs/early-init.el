;;; early-init.el -*- lexical-binding: t; -*-
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; tune gc for better performance
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original
                  gc-cons-threshold (* 8 1024 1024) ; 8MB
                  gc-cons-percentage 0.1)
            (garbage-collect) t))
(run-with-idle-timer 5 t 'garbage-collect)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

(unless (file-directory-p (expand-file-name "cache" user-emacs-directory))
  (make-directory (expand-file-name "cache" user-emacs-directory)))

(setq package-quickstart t
      package-quickstart-file (expand-file-name "cache/package-quickstart.el" user-emacs-directory))

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)

(set-face-attribute 'default nil        :family "Victor Mono"     :height 110 :weight 'medium)
(set-face-attribute 'fixed-pitch nil    :family "Victor Mono"     :height 110 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "Liberation Sans" :height 110 :weight 'normal)

; (add-to-list 'default-frame-alist '(background-color . "white"))

(provide 'early-init)
;;; early-init.el ends here
