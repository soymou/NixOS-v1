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

(defvar lean4-mode-required-packages nil)
(use-package lean4-mode
  :init
  ;; Enable lsp-mode support for Lean 4
  (setq lean4-mode-required-packages '(dash flycheck f s lsp-mode))
  :hook (lean4-mode . lsp-deferred))

(use-package idris2-mode
  :mode "\\.idr\\'"
  :hook (idris2-mode . lsp-deferred))

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
