;;; dev.el --- Development & Tools -*- lexical-binding: t; -*-

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))

;; Eglot (LSP) - Built-in since Emacs 29, but use-package handles config well
(use-package eglot
  :hook ((python-mode . eglot-ensure)
	 (rust-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
	 (js-mode . eglot-ensure)
	 (typescript-mode . eglot-ensure)))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Vterm: The best terminal emulator in Emacs
(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell (executable-find "fish"))
  (setq vterm-max-scrollback 10000)
  ;; Open vterm in the current window's directory
  (setq vterm-always-compile-module t)
  
  ;; Evil integration for Vterm
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'insert))
  
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state))))

;; Vterm Toggle: Show/Hide terminal
(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.3))))

;; Apheleia: Async code formatting
(use-package apheleia
  :init (apheleia-global-mode +1))

;; Restore gc-cons-threshold after startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 2 1000 1000))))

(provide 'dev)