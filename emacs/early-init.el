;; early-init.el -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(setq frame-inhibit-implied-resize t)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)


