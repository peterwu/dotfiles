;;; my-ezf.el -*- lexical-binding: t; -*-
(require 'ido)

(defcustom my-ezf-window-name "ezf"
  "ezf default window name")

(defcustom my-ezf-field 1
  "ezf default field")

(defcustom my-ezf-delimiter " "
  "ezf default delimiter")

(defcustom my-ezf-prompt "Run: "
  "ezf default-prompt")

(defun my-ezf-launcher (filename &optional field delimiter prompt)
  (let* ((field (or field my-ezf-field))
         (delimiter (or delimiter my-ezf-delimiter))
         (prompt (or prompt my-ezf-prompt))
         (lines (with-temp-buffer
                  (insert-file-contents filename nil)
                  (string-lines (buffer-string) t)))
         (menu (mapcar (lambda (line)
                         (string-join (nthcdr field (string-split line delimiter)) delimiter))
                       lines))
         (selected (ido-completing-read prompt menu)))

    (catch 'found
      (dolist (line lines)
        (when (string= selected
                       (string-join (nthcdr field (string-split line delimiter)) delimiter))
          (throw 'found line))))))

(defun my-ezf (filename &optional field delimiter prompt)
  "Complete candidates in FILENAME with `ido-completing-read'."
  (interactive)
  (with-selected-frame
      (make-frame `((name . ,my-ezf-window-name)
                    (minibuffer . only)
                    (fullscreen . nil)
                    (undecorated . t)
                    (auto-raise . t)
                    (background-color . "white smoke")
                    (width . 80)
                    (height . 11)))
    (unwind-protect
        (let ((inhibit-message t))
          (my-ezf-launcher filename field delimiter prompt))
      (delete-frame))))

(provide 'my-ezf)
