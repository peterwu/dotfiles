(let ((my-packages (locate-user-emacs-file "my-packages.el")))
  (when (file-exists-p my-packages)
    (load-file my-packages)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(let ((my-settings (locate-user-emacs-file "my-settings.el")))
  (when (file-exists-p my-settings)
    (load-file my-settings)))
