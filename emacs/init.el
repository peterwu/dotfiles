;;; init.el --- Initialization file for Emacs
(require 'package)
(require 'org)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; install use-package if not
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
