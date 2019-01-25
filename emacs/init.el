;;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
