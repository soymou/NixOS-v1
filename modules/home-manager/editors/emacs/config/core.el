;;; core.el --- Core Settings & Package Management -*- lexical-binding: t; -*-

;; tune gc for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil)

;; GCMH: Garbage Collector Magic Hack
(use-package gcmh
  :init
  (gcmh-mode 1))

;; Ai integration
(use-package ai-code
  :config
  (ai-code-set-backend 'gemini-cli)
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1)
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

;; Clean up the working directory (Move backups and auto-saves to /tmp)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq lock-file-name-transforms `((".*" ,temporary-file-directory t)))
;; Also disable lock-files entirely if you prefer (stops the .#file files)
(setq create-lockfiles nil)

(provide 'core)
