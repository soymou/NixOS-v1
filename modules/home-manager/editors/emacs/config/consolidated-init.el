;;; consolidated-init.el --- Complete Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated configuration file that includes all modules inline.
;; This is needed for emacsWithPackagesFromUsePackage to work properly.

;;; Code:

;; Don't auto-install packages - they're provided by Nix
(setq use-package-always-ensure nil)

;;==============================================================================
;; Evil Mode Configuration
;;==============================================================================

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; Evil Collection for better Evil integration
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;==============================================================================
;; UI Configuration
;;==============================================================================

(use-package all-the-icons)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-unicode-fallback nil
        doom-modeline-minor-modes nil))

;;==============================================================================
;; Completion System
;;==============================================================================

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))

(use-package company
  :init
  (global-company-mode 1)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-show-numbers t))

;;==============================================================================
;; LSP Configuration
;;==============================================================================

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;;==============================================================================
;; Flycheck Configuration
;;==============================================================================

(use-package flycheck
  :init
  (global-flycheck-mode))

;;==============================================================================
;; General Keybinding System
;;==============================================================================

(use-package general
  :config
  
  ;; Leader key setup
  (general-create-definer leader-key
    :keymaps '(normal visual emacs)
    :prefix "SPC")

  ;; Main leader bindings
  (leader-key
    ;; Quick actions
    "."  '(find-file :which-key "Find file")
    "q"  '(kill-buffer-and-window :which-key "Kill buffer and window")
    
    ;; Aidermacs
    "a" '(:ignore t :which-key "Aidermacs")
    "aa" '(aidermacs-transient-menu :which-key "Transient menu")
    
    ;; Toggle commands
    "t" '(:ignore t :which-key "Toggle")
    "tt"  '(vterm-toggle :which-key "Toggle vterm")
    
    ;; File commands
    "f"  '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")

    ;; Buffer commands
    "b"  '(:ignore t :which-key "Buffers")
    "bs" '(switch-to-buffer :which-key "Switch buffer")
    "bk" '(kill-buffer :which-key "Kill buffer (select)")
    "bd" '((lambda () (interactive)
             (kill-buffer (current-buffer)))
           :which-key "Kill current buffer")
    "be" '(eval-buffer :which-key "Eval buffer")))

;;==============================================================================
;; Which-key integration
;;==============================================================================

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))

;;==============================================================================
;; Terminal Integration
;;==============================================================================

(use-package vterm)
(use-package vterm-toggle)

;;==============================================================================
;; Org Mode Configuration
;;==============================================================================

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

;;==============================================================================
;; AI Integration
;;==============================================================================

(use-package aidermacs)
(use-package transient)

;;==============================================================================
;; Programming Languages
;;==============================================================================

(use-package markdown-mode)
(use-package typst-ts-mode)

;;==============================================================================
;; Version Control
;;==============================================================================

(use-package magit)

;;==============================================================================
;; Basic UI Settings
;;==============================================================================

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Better scrolling
(setq scroll-conservatively 101
      scroll-margin 3
      scroll-preserve-screen-position t)

;; Disable menu bar, tool bar, and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show matching parentheses
(show-paren-mode 1)

;;; consolidated-init.el ends here
