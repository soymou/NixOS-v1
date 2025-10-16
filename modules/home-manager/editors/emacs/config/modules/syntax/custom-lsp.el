    
;;; custom-lsp.el --- Lsp mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for lsp mode package.

;;; Code:
(use-package lsp-mode)

(use-package lsp-nix
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred))

(provide 'modules/syntax/custom-lsp)
;;; custom-lsp.el ends here
