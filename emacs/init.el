(let ((my-packages "~/.emacs.d/my-packages.el"))
  (when (file-exists-p my-packages)
    (load-file my-packages)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(let ((my-settings "~/.emacs.d/my-settings.el"))
  (when (file-exists-p my-settings)
    (load-file my-settings)))
