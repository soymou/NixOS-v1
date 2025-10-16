;;; custom-evil.el --- Evil Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Evil mode and Evil Collection.
;; Provides Vim-like keybindings and behavior in Emacs.

;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)

;; Evil Collection for better Evil integration
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'custom-evil)
;;; custom-evil.el ends here
