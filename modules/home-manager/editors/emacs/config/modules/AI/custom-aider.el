;;; custom-aider.el --- Custom aider configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Aidermacs configuration

;;; Code:

(defun get-open-router-api-key ()
(shell-command-to-string "echo -n $OPENROUTER_API_KEY"))

(use-package aidermacs
  :config
  (setenv "OPENROUTER_API_KEY" (get-open-router-api-key))
  :custom
  (aidermacs-default-model "openrouter/anthropic/claude-sonnet-4.5"))

(provide 'modules/AI/custom-aider)
;;; custom-aider.el ends here
