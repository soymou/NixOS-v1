;;; custom-flycheck.el --- Flycheck Configuration -*- lexical-keybinding: t; -*-

;;; Comentary:
;; Configuration for flyckeck

;;; Code:

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'custom-flycheck)
;;; custom-flycheck ends here
