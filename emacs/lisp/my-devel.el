;;; my-lang.el -*- lexical-binding: t; -*-

;; expand macros
(bind-key "RET" #'pp-macroexpand-last-sexp my-ctl-z-map)

;; globals
(use-package eglot
  :hook
  ((c-ts-mode
    c++-ts-mode
    go-ts-mode
    python-ts-mode) . eglot-ensure))

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
  (:map my-ctl-z-!-map
        ("s" . flymake-start)
        ("d" . flymake-show-diagnostics-buffer)
        ("n" . flymake-goto-next-error)
        ("p" . flymake-goto-prev-error)))

;; c/c++
(use-package clang-format
  :if (file-executable-p "/usr/bin/clang-format")
  :preface
  (defvar my-clang-format-style "file")
  (defvar my-clang-format-fallback-style
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
  (gdb-debuginfod-enable-setting nil)
  (gdb-many-windows t)
  (gdb-non-stop-setting nil)
  (gdb-restore-window-configuration-after-quit t)
  (gdb-show-main t))

;; python
(use-package python
  :preface
  (defconst my-pyvenv-dirs '(".venv" "venv"))
  (defvar my-pyvenv-virtual-env nil)

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
         (match (not (equal venv-dir my-pyvenv-virtual-env))))

      (let ((venv-bin-dir
             (concat (file-name-as-directory venv-dir) "bin")))
        (setenv "VIRTUAL_ENV" venv-dir)
        (setenv "PYTHONHOME" nil)
        (setq exec-path (append `(,venv-bin-dir) exec-path))
        (setq my-pyvenv-virtual-env venv-dir))))
  :custom
  (python-check-command "pylint")
  :hook
  (python-mode . my-pyvenv-auto-activate))

(require 'treesit)
(setq-default treesit-language-source-alist
              '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                (c "https://github.com/tree-sitter/tree-sitter-c")
                (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                (go "https://github.com/tree-sitter/tree-sitter-go")
                (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
                (json "https://github.com/tree-sitter/tree-sitter-json")
                (make "https://github.com/alemuller/tree-sitter-make")
                (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                (python "https://github.com/tree-sitter/tree-sitter-python")
                (toml "https://github.com/tree-sitter/tree-sitter-toml")
                (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setopt major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (cpp-mode . cpp-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (yaml-mode . yaml-ts-mode)))

(when (treesit-ready-p 'go)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

(provide 'my-devel)
