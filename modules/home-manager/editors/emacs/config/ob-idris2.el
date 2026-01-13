;;; ob-idris2.el --- Org-babel functions for Idris 2 evaluation -*- lexical-binding: t; -*-

(require 'ob)

(defvar org-babel-default-header-args:idris2
  '((:results . "output") (:exports . "both")))

(defun org-babel-execute:idris2 (body params)
  "Execute a block of Idris 2 code with org-babel."
  (let* ((temp-file (org-babel-temp-file "idris2-" ".idr"))
         (temp-dir (file-name-directory temp-file))
         (temp-filename (file-name-nondirectory temp-file))
         (cmd (format "cd %s && idris2 --no-color %s --exec main"
                      (org-babel-process-file-name temp-dir)
                      (org-babel-process-file-name temp-filename))))
    (with-temp-file temp-file
      (unless (string-match-p "^module " body)
        (insert "module Main\n\n"))
      (insert body))
    (org-babel-eval cmd "")))

(provide 'ob-idris2)

