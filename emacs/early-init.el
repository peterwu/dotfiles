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
	    (garbage-collect)
	    (run-with-idle-timer 5 t 'garbage-collect)))

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; disable some modes
(display-battery-mode 0)
(display-time-mode 0)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode 0))

;; move state files off to .cache folder
(setq save-place-file (expand-file-name ".cache/places" user-emacs-directory)
      recentf-save-file (expand-file-name ".cache/recentf" user-emacs-directory)
      bookmark-default-file (expand-file-name ".cache/bookmarks" user-emacs-directory)
      lsp-session-file (expand-file-name ".cache/lsp-session" user-emacs-directory)
      tramp-persistency-file-name (expand-file-name
				   ".cache/tramp" user-emacs-directory))

  ;; set default face
(set-face-attribute 'default nil
                    :family "Iosevka Fusion"
                    :foundry "outline"
                    :height 130)

(provide 'early-init)
;;; early-init.el ends here
