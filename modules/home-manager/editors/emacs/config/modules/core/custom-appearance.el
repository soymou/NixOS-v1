;;; custom-appearance.el --- Theme and Modeline Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for themes and the modeline.
;; Uses Doom themes and Doom modeline for a modern look.

;;; Code:

;; Modeline
(use-package doom-modeline
  :config
  (doom-modeline-mode))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(provide 'modules/core/custom-appearance)
;;; custom-appearance.el ends here
