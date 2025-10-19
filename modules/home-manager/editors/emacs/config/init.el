;;; config.el --- Main Configuration Entry Point -*- lexical-binding: t; -*-

;;; Commentary:
;; Main configuration file that loads all modular components.
;; Each module focuses on a specific functionality area.

;;; Code:

;; Don't auto-install packages - they're provided by Nix
(setq use-package-always-ensure nil)

;; Add the configuration directories to load-path
(let ((config-dir (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name "modules" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/core" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/syntax" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/languages" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/org" config-dir))
  (add-to-list 'load-path (expand-file-name "modules/AI" config-dir)))

;; Load configuration modules
(require 'custom-evil)        ; Evil mode (Vim keybindings)
(require 'custom-keybinds)    ; Custom keybindings
(require 'custom-ui)          ; Basic UI improvements
(require 'custom-appearance)  ; Themes and modeline
(require 'custom-completion)  ; Ivy/Counsel completion
(require 'custom-treesit)     ; Tree-sitter configuration
(require 'custom-lsp)         ; Lsp config
(require 'custom-flycheck)    ; Flycheck config
;; (require 'custom-spell)       ; Enable spell checking - disabled until hunspell is configured
(require 'custom-mdx)         ; MDX file support
(require 'custom-babel)       ; Load babel languages
(require 'custom-org)         ; Load org-mode configuration
(require 'custom-aider)

;; Run startup apps
(set-babel-languages languages)

;;; config.el ends here
