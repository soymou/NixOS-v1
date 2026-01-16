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
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-mouse t))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package consult-lsp
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)))

(defvar lean4-mode-required-packages nil)
(use-package lean4-mode
  :init
  ;; Enable lsp-mode support for Lean 4
  (setq lean4-mode-required-packages '(dash flycheck f s lsp-mode))
  :hook (lean4-mode . lsp-deferred))

(use-package clojure-mode
  :hook ((clojure-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred))
  :config
  (setq clojure-align-forms-automatically t))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred))

(use-package python
  :ensure nil
  :hook (python-mode . lsp-deferred))

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

;; LaTeX
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'lsp-deferred)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" "TeX-pdfview-sync-view"))
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :hook (org-mode . turn-on-org-cdlatex))

(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))

(provide 'lang)
