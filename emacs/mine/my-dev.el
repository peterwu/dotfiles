;;; my-lang.el -*- lexical-binding: t; -*-

;; globals
(use-package eglot
  :hook
  ((c-mode c++-mode python-mode) . eglot-ensure))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-no-changes-timeout nil)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-suppress-zero-counters t)
  (flymake-wrap-around nil)
  :bind
  ("C-c ! s" . flymake-start)
  ("C-c ! d" . flymake-show-diagnostics-buffer)
  ("C-c ! n" . flymake-goto-next-error)
  ("C-c ! p" . flymake-goto-prev-error))

;; c/c++
(use-package clang-format
  :if (file-executable-p "/usr/bin/clang-format")
  :preface (defvar my-clang-format-style "file")
  :preface (defvar my-clang-format-fallback-style
             (concat "{"
                     "BasedOnStyle: LLVM,"
                     "Language: Cpp,"

                     "AccessModifierOffset: -4,"
                     "IndentWidth: 4,"
                     "UseTab: Never,"

                     "AllowShortFunctionsOnASingleLine: Empty,"
                     "AllowShortLambdasOnASingleLine: Empty,"
                     "AlwaysBreakTemplateDeclarations: Yes,"
                     "BreakBeforeBraces: Stroustrup,"
                     "BreakConstructorInitializers: BeforeComma,"
                     "IndentPPDirectives: AfterHash,"
                     "PointerAlignment: Left"
                     "}"))
  :hook
  ((c-mode c++-mode) .
   (lambda ()
     (add-hook 'before-save-hook
               (lambda ()
                 (if (locate-dominating-file "." ".clang-format")
                     (clang-format-buffer my-clang-format-style)
                   (clang-format-buffer my-clang-format-fallback-style))
                 nil)
               nil
               t))))

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-restore-window-configuration-after-quit t)
  (gdb-show-main t))

;; python
(use-package python
  :custom
  (python-check-command "pylint"))

(use-package pyvenv
  :ensure t
  :demand t
  :preface
  (defconst my-pyvenv-dirs '(".venv" "venv"))
  (defun my-pyvenv-auto-activate ()
    (when-let
        ((venv-dir
          (seq-find #'identity
                    (mapcar (lambda (dir)
                              (let
                                  ((parent-dir
                                    (locate-dominating-file
                                     default-directory
                                     (concat
                                      (file-name-as-directory
                                       (concat
                                        (file-name-as-directory dir)
                                        "bin"))
                                      "activate"))))
                                (when parent-dir
                                  (concat
                                   (file-name-as-directory parent-dir)
                                   dir))))
                            my-pyvenv-dirs)))
         (match (not (equal venv-dir pyvenv-virtual-env))))
      (pyvenv-activate venv-dir)
      (message "pyvenv activated %s." venv-dir)))
  :hook
  (python-mode . my-pyvenv-auto-activate))

(provide 'my-dev)
