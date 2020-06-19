;;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
(setq vc-follow-symlinks nil)
(setq initial-major-mode 'org-mode)

(require 'package)
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
