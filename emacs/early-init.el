;;; early-init.el -*- lexical-binding: t; -*-

;; tune gc for better performance
;; check gcs-done and gc-elapsed right after startup
(setopt my-file-name-handlers file-name-handler-alist
        file-name-handler-alist nil
        gc-cons-threshold most-positive-fixnum)

;; do not resize the frame at this early stage.
(setopt frame-inhibit-implied-resize t)

;; prefer loading newer compiled files
(setopt load-prefer-newer t )

;; no litterring
(make-directory (expand-file-name "cache" user-emacs-directory) t)

(setopt package-quickstart t
        package-quickstart-file (expand-file-name
                                 "cache/package-quickstart.el"
                                 user-emacs-directory))

;; custom.el
(setopt custom-file (make-temp-file "emacs-custom-" nil ".el"))

;; clean GUI
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'context-menu-mode)
  (context-menu-mode +1))

;; make native compilation silent
(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors 'silent))

;; zen style experience
(setopt use-dialog-box nil)
(setopt use-file-dialog nil)
(setopt use-short-answers t)
(setopt visible-bell t)

;; disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; faster redisplay
(setq bidi-inhibit-bpa t)

(set-face-attribute 'default        nil :family "SF Mono" :height 160 :weight 'normal)
(set-face-attribute 'fixed-pitch    nil :family "SF Mono" :height 160 :weight 'normal)
(set-face-attribute 'variable-pitch nil :family "SF Pro"  :height 160 :weight 'normal)

(provide 'early-init)
;;; early-init.el ends here
