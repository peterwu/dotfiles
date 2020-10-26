;;;; init.el --- Initialization file for Emacs


(require 'package)
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

