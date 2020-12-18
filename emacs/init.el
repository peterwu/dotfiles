;;; init.el --- Initialization file for Emacs
(require 'package)
(require 'org)

(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
