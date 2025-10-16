;;; config.el --- Main Configuration Entry Point -*- lexical-binding: t; -*-

;;; Commentary:
;; Main configuration file that loads all modular components.
;; Each module focuses on a specific functionality area.

;;; Code:

;; Don't auto-install packages - they're provided by Nix
(setq use-package-always-ensure nil)

;; Add the configuration directories to load-path
(let ((config-dir (file-name-directory load-file-name)))
  (add-to-list 'load-path config-dir)
  (add-to-list 'load-path (expand-file-name "modules" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/core" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/syntax" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/languages" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/org" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/AI" config-dir)))

;; Load configuration modules
(require 'modules/core/custom-evil)        ; Evil mode (Vim keybindings)
(require 'modules/core/custom-keybinds)    ; Custom keybindings
(require 'modules/core/custom-ui)          ; Basic UI improvements
(require 'modules/core/custom-appearance)  ; Themes and modeline
(require 'modules/syntax/custom-completion)  ; Ivy/Counsel completion
(require 'modules/syntax/custom-treesit)     ; Tree-sitter configuration
(require 'modules/syntax/custom-lsp)         ; Lsp config
(require 'modules/syntax/custom-flycheck)    ; Flycheck config
(require 'modules/syntax/custom-spell)       ; Enable spell checking
(require 'modules/languages/custom-mdx)         ; MDX file support
(require 'modules/org/custom-babel)       ; Load babel languages
(require 'modules/org/custom-org)         ; Load org-mode configuration
(require 'modules/AI/custom-aider)

;; Run startup apps
(set-keybinds)
(set-babel-languages languages)

;;; config.el ends here
