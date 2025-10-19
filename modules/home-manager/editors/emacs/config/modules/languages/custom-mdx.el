;;; custom-mdx.el --- Custom mdx mode  -*-  lexical-binding: t; -*-
;;; Commentary:
;; Configuration for MDX files (Markdown with JSX) for Astro projects.
;; MDX allows you to write JSX/React components within Markdown content.

;;; Code:

;; Base configuration using markdown-mode for MDX files
(use-package markdown-mode
  :mode (("\\.mdx\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t)
  (setq markdown-enable-wiki-links t)

  ;; Enable syntax highlighting for common languages in code blocks
  (setq markdown-code-lang-modes
        '(("jsx" . js-mode)
          ("tsx" . typescript-mode)
          ("js" . js-mode)
          ("javascript" . js-mode)
          ("typescript" . typescript-mode)
          ("ts" . typescript-mode)
          ("python" . python-mode)
          ("py" . python-mode)
          ("shell" . sh-mode)
          ("bash" . sh-mode)
          ("json" . js-json-mode)
          ("css" . css-mode)
          ("html" . html-mode))))

;; Add flycheck support for MDX if available
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'markdown-mode))

;; Hook to enable additional features in MDX files
(defun mdx-mode-hook ()
  "Custom hook for MDX files."
  ;; Enable auto-pairing for JSX brackets
  (electric-pair-local-mode 1)

  ;; Enable company completion if available
  (when (fboundp 'company-mode)
    (company-mode 1))

  ;; Enable LSP for MDX files if lsp-mode is available
  (when (and (fboundp 'lsp-deferred)
             (or (executable-find "typescript-language-server")
                 (executable-find "vscode-langservers-extracted")))
    (lsp-deferred)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (when (string-match-p "\\.mdx\\'" (buffer-file-name))
              (mdx-mode-hook))))

;; Optional: Add custom snippets or abbreviations for MDX
(with-eval-after-load 'markdown-mode
  ;; Add custom keybindings for MDX if needed
  (define-key markdown-mode-map (kbd "C-c C-x i")
    (lambda ()
      (interactive)
      (insert "import  from ''\n")
      (forward-line -1)
      (forward-char 7)))

  (define-key markdown-mode-map (kbd "C-c C-x c")
    (lambda ()
      (interactive)
      (insert "<>\n</>")
      (forward-line -1)
      (forward-char 1))))

(provide 'custom-mdx)
;;; custom-mdx.el ends here
