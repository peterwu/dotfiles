(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(setq backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)
(setq scroll-step 1
      scroll-margin 2
      scroll-conservatively 9999
      auto-window-vscroll nil)
(setq vc-follow-symlinks nil)
(setq delete-by-moving-to-trash t)
(global-hl-line-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key [f1] 'eshell)
