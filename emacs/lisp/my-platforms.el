;;; my-platforms.el -*- lexical-binding: t; -*-

(defun my-set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string
           "$SHELL --login -c 'echo $PATH' 2> /dev/null"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(cond
 ((eq system-type 'windows-nt)
  (setopt set-selection-coding-system 'utf-16le-dos)
  (setopt w32-get-true-file-attributes nil)
  (setq-default inhibit-compacting-font-caches t))
 ((eq system-type 'darwin)
  (my-set-exec-path-from-shell-PATH)
  (setopt insert-directory-program "gls")
  (setopt mac-option-modifier 'meta)))

(provide 'my-platforms)
