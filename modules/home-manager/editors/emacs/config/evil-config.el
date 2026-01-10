;;; evil-config.el --- Evil Mode & Keybindings -*- lexical-binding: t; -*-

;; General.el: Leader key configuration
(use-package general
  :config
  (general-def
    :prefix "SPC"
    :states '(normal visual motion)
    :keymaps 'override
    "." '(dired :which-key "dired")

    "o" '(:ignore t :which-key "org-mode")
    "ot" '(org-babel-tangle :which-key "tangle current file")
    
    "b" '(:ignore t :which-key "buffer")
    "be" '(eval-buffer :which-key "evaluate buffer")
    
    "f"  '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")
    "fr" '(consult-recent-file :which-key "recent files")

    "b"  '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bk" '(kill-current-buffer :which-key "kill buffer")
    "br" '(revert-buffer :which-key "revert buffer")

    "u"  '(vundo :which-key "undo tree")

    "t"  '(:ignore t :which-key "tabs")
    "tn" '(centaur-tabs--create-new-tab :which-key "new tab")
    "tl" '(centaur-tabs-forward :which-key "next tab")
    "th" '(centaur-tabs-backward :which-key "prev tab")
    "ts" '(centaur-tabs-forward-group :which-key "switch group")
    "tk" '(kill-current-buffer :which-key "close tab")

    "w"  '(:ignore t :which-key "window")
    "wl" '(windmove-right :which-key "move right")
    "wh" '(windmove-left :which-key "move left")
    "wk" '(windmove-up :which-key "move up")
    "wj" '(windmove-down :which-key "move down")
    "w/" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split bottom")
    "wd" '(delete-window :which-key "delete window")

    "p"  '(:ignore t :which-key "project")
    "pf" '(projectile-find-file :which-key "find file")
    "ps" '(projectile-switch-project :which-key "switch project")
    "pp" '(projectile-switch-project :which-key "switch project")

    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "status")
    "gd" '(magit-dispatch :which-key "dispatch")
    "gf" '(magit-file-dispatch :which-key "file dispatch")
    "gb" '(magit-branch-checkout :which-key "branch/checkout")
    "gc" '(magit-commit :which-key "commit")
    "gl" '(magit-log-all :which-key "log")
    "gP" '(magit-push :which-key "push")
    "gp" '(magit-pull :which-key "pull")

    "v"  '(:ignore t :which-key "vterm")
    "vv" '(vterm :which-key "vterm")
    "vs" '(vterm-other-window :which-key "vterm split")

    "?"  '(which-key-show-top-level :which-key "show all keys")

    "h"  '(:ignore t :which-key "help")
    "hv" '(helpful-variable :which-key "variable")
    "hf" '(helpful-callable :which-key "function")
    "hk" '(helpful-key :which-key "key")))

;; Evil: Vim emulation
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil Collection: Evil bindings for the rest of Emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Avy: Jump to things
(use-package avy
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2))
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g s") 'avy-goto-char-timer)))

;; Vundo: Visual undo tree
(use-package vundo
  :commands (vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(provide 'evil-config)