;;; ai.el --- AI Coding Configuration -*- lexical-binding: t; -*-

(use-package ai-code
  :config
  ;; Set it as the default backend
  (ai-code-set-backend 'gemini)

  ;; Eat terminal configuration
  (use-package eat
    :config
    ;; Ensure ESC is sent to the terminal in semi-char mode
    ;; This allows stopping gemini-cli generation
    (define-key eat-semi-char-mode-map (kbd "<escape>") #'eat-self-input)
    
    ;; Evil integration
    (with-eval-after-load 'evil
      (evil-set-initial-state 'eat-mode 'insert)))

  (provide 'ai)
