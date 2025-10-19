;;; custom-completion.el --- Completion Framework Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Ivy and Counsel completion frameworks.
;; Provides enhanced minibuffer completion and M-x functionality.

;;; Code:

;; Better completion
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

;; Enhanced M-x
(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))

(global-set-key (kbd "s-<tab>") 'counsel-switch-buffer)
(global-set-key (kbd "s-SPC") 'counsel-linux-app)

;; Company
(use-package company
  :init
  (global-company-mode 1)
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 1))

(provide 'custom-completion)
;;; custom-completion.el ends here
