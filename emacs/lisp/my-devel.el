;;; my-devel.el -*- lexical-binding: t; -*-

;; globals
(use-package dape
  :ensure t
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-default-breakpoints-file (expand-file-name
                                  "cache/dape-breakpoints"
                                  user-emacs-directory))
  :hook
  (after-init . dape-breakpoint-load)
  (kill-emacs . dape-breakpoint-save)

  (dape-compile . kill-buffer)
  (dape-display-source . pulse-momentary-highlight-one-line)
  :config
  (dape-breakpoint-global-mode +1))

(use-package eglot
  :custom
  (eglot-code-action-indications '(eldoc-hint))
  :hook
  ((c-ts-mode
    c++-ts-mode
    cmake-ts-mode
    go-ts-mode
    python-ts-mode) . eglot-ensure)

  (before-save
   . (lambda ()
       (when (eglot-managed-p)
         (eglot-format-buffer))))
  :bind
  (:map my-C-z-l-map
        ("D" . eglot-find-declaration)
        ("F" . eglot-format-buffer)
        ("R" . eglot-rename)
        ("a" . eglot-code-actions)
        ("d" . xref-find-definitions)
        ("f" . eglot-format)
        ("h" . eldoc)
        ("i" . eglot-find-implementation)
        ("r" . xref-find-references)
        ("t" . eglot-find-typeDefinition)))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-suppress-zero-counters t)
  (flymake-wrap-around nil)
  :bind
  (:map my-C-z-!-map
        ("s" . flymake-start)
        ("d" . flymake-show-buffer-diagnostics)
        ("D" . flymake-show-project-diagnostics)
        ("n" . flymake-goto-next-error)
        ("p" . flymake-goto-prev-error)))

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package gdb-mi
  :custom
  (gdb-debuginfod-enable-setting nil)
  (gdb-many-windows t)
  (gdb-non-stop-setting nil)
  (gdb-restore-window-configuration-after-quit t)
  (gdb-show-main t))

(use-package treesit
  :custom
  (treesit-enabled-modes t))

(use-package markdown-ts-mode
  :mode (rx ".md" string-end))

(use-package markdown-ts-mode-x
  :custom
  (markdown-ts-convert-display-function #'browse-url-of-file))

(provide 'my-devel)
