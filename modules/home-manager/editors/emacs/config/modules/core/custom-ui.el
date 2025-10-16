;;; custom-ui.el --- Basic UI Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic UI improvements and settings.
;; Includes disabling unnecessary UI elements and enabling helpful features.

;;; Code:

;; Disable menu bar, tool bar, and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Enable systemtray
;;(require 'exwm-systemtray)
;;(exwm-systemtray-mode)

;; Dashboard
(use-package dashboard
  :config
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-startup-banner 1)
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))


;; Show line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Which-key: shows available keybindings
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(provide 'custom-ui)
;;; custom-ui.el ends here
