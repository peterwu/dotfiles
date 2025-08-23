;;; my-lang.el -*- lexical-binding: t; -*-

;; expand macros
(bind-key "RET" #'pp-macroexpand-last-sexp my-ctl-z-map)

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
  :config
  (setf (alist-get '(cmake-mode cmake-ts-mode) eglot-server-programs)
        (eglot-alternatives
         '(("neocmakelsp" "--stdio") "cmake-language-server")))
  :hook
  ((c-ts-mode
    c++-ts-mode
    cmake-ts-mode
    go-ts-mode
    python-ts-mode) . eglot-ensure)
  :bind
  (:map my-ctl-z-l-map
        ("D" . eglot-find-declaration)
        ("F" . eglot-format-buffer)
        ("R" . eglog-rename)
        ("a" . eglot-code-actions)
        ("d" . xref-find-definitions)
        ("f" . eglot-format)
        ("h" . eldoc)
        ("i" . eglot-find-implementation)
        ("r" . xref-find-referenes)
        ("t" . eglot-find-typeDefinition)))

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
  :if (file-executable-p "clang-format")
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
  (defun my-uv-activate ()
    "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter."
    (interactive)
    (let* ((project-root (project-root (project-current t)))
           (venv-path (expand-file-name ".venv" project-root))
           (python-path (expand-file-name
                         (if (eq system-type 'windows-nt)
                             "Scripts/python.exe"
                           "bin/python")
                         venv-path)))
      (if (file-exists-p python-path)
          (progn
            (setopt python-shell-interpreter python-path)
            (setopt python-shell-virtualenv-root venv-path)

            (let ((venv-bin-dir (file-name-directory python-path)))
              (setopt exec-path (cons venv-bin-dir
                                      (remove venv-bin-dir exec-path))))

            (setenv "PATH" (concat (file-name-directory python-path)
                                   path-separator
                                   (getenv "PATH")))
            (setenv "VIRTUAL_ENV" venv-path)
            (setenv "PYTHONHOME" nil)

            (message "Activated UV Python environment at %s" venv-path))
        (message "No UV Python environment found in %s" project-root))))
  :custom
  (python-check-command "ruff check --select ALL")
  :hook
  ((python-mode python-ts-mode) . my-uv-activate))

(use-package treesit
  :init
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
  :custom
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (sh-mode . bash-ts-mode)
     (yaml-mode . yaml-ts-mode)))
  :mode ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)
  :mode ("\\.go\\'" . go-ts-mode))

(provide 'my-devel)
