;;; custom-treesit.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for tree-sitter integration in Emacs.
;; Sets up grammar loading paths and major mode remappings.

;;; Code:

;; Configure tree-sitter grammar paths from NixOS
(when (boundp 'treesit-grammars-path)
  (setq treesit-extra-load-path (list treesit-grammars-path)))

;; Auto-remap major modes to use tree-sitter versions when available
(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (typst-mode . typst-ts-mode)))

;; Configure TypeScript and TSX modes
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Configure Typst mode
(use-package typst-ts-mode
  :mode "\\.typ\\'")

(provide 'custom-treesit)
;;; custom-treesit.el ends here
