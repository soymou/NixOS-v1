;;; lang.el --- Programming Languages -*- lexical-binding: t; -*-

;; Spelling
(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode))
  :custom
  (ispell-program-name "hunspell")
  :config
  (add-to-list 'ispell-local-dictionary-alist
	       '("en_US,es_MX" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,es_MX") nil utf-8))
  (setq ispell-local-dictionary "en_US,es_MX")
  (setq ispell-dictionary "en_US,es_MX"))

;; Languages
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lean4-mode
  :init
  ;; Enable lsp-mode support for Lean 4
  (setq lean4-mode-required-packages '(dash flycheck f s lsp-mode))
  :hook (lean4-mode . lsp-deferred))

(provide 'lang)
