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

(provide 'core)
