;; variables
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(setq backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)
(setq scroll-step 1
      scroll-margin 2
      scroll-conservatively 10000
      auto-window-vscroll nil)
(setq vc-follow-symlinks nil)
(setq delete-by-moving-to-trash t)
(setq default-input-method "chinese-py")
(setq display-line-numbers-type 'relative)

;; functions
(fset 'yes-or-no-p 'y-or-n-p)

;; modes
(global-visual-line-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(electric-pair-mode t)
(display-battery-mode t)
(display-time-mode t)
(show-paren-mode t)
(size-indication-mode t)
(global-display-line-numbers-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; set default font
(set-face-attribute 'default t
		    :family "Fira Code Retina"
		    :foundry "outline"
		    :slant 'normal
		    :weight 'normal
		    :height 120
		    :width 'normal)

;; (set-face-background 'line-number (color-darken-name (face-attribute 'default :background) 2))
(set-face-foreground 'line-number-current-line "#F1FA8C")

;; key bindings
(global-set-key [f1] 'eshell)
