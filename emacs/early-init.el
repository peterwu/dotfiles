;; early-init.el -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq site-run-file nil)

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache.
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; disable some modes
(display-battery-mode 0)
(display-time-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))

;; move state files off to .cache folder
(setq save-place-file (expand-file-name ".cache/places" user-emacs-directory)
      recentf-save-file (expand-file-name ".cache/recentf" user-emacs-directory)
      bookmark-default-file (expand-file-name ".cache/bookmarks" user-emacs-directory)
      lsp-session-file (expand-file-name ".cache/lsp-session" user-emacs-directory)
      tramp-persistency-file-name (expand-file-name
				   ".cache/tramp" user-emacs-directory))
