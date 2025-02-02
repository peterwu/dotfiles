;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(use-package my-defaults)
(use-package my-platforms)
(use-package my-mode-line)
(use-package my-theme)

(use-package my-keymaps)
(use-package my-clipboard)
(use-package my-evil)
(use-package my-numbers)
(use-package my-vc)

(use-package my-devel)
(use-package my-dired)
(use-package my-org)
(use-package my-speedbar)
(use-package my-window)

(use-package my-packages)

(provide 'init)
;;; init.el ends here
