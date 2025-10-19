;;; custom-org.el --- Org-mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Custom org-mode configuration

;;; Code:

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("le" . "src lean4"))
(add-to-list 'org-structure-template-alist '("nix" . "src nix"))
(provide 'custom-org)
;;; custom-org.el ends here
