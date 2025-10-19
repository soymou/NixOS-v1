;;; custom-babel.el --- Custom babel languages -*- lexical-binding: t; -*-
;;; Commentary:
;; This is a custom configuration for babel languages

;;; Code:

(defvar languages '(python))

(defun consume (string)
  (split-string string))  ; No need for format here since split-string already returns a list

(defun format-list (list)
  (let ((lista '()))               ; Initialize an empty list `lista`
    (dolist (item list lista)       ; Iterate over `list`, adding `(item . t)` pairs
      (setq lista (cons (cons item t) lista)))  
    (nreverse lista)))                        ; Return the formatted list, reversed to maintain original order

(defun set-babel-languages (languages)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (format-list languages)))

(setq org-confirm-babel-evaluate nil)

(provide 'custom-babel)
;;; custom-babel.el ends here
