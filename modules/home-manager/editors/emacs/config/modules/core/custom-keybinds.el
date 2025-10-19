;;; custom-keybinds.el --- Custom keybinds -*- lexical-binding: t; -*-

;;; Commentary:
;; Keybinding configuration using general.el and which-key.
;; Includes leader key mappings and EXWM workspace management.

;;; Code:

;;------------------------------------------------------------------------------
;; Workspace Management
;;------------------------------------------------------------------------------

(defun move-to-workspace (id)
  "Move the current buffer or EXWM window to workspace ID and switch to it."
  (interactive "nWorkspace ID: ")
  (if (eq major-mode 'exwm-mode)
      (exwm-workspace-move-window id)
    ;; For non-EXWM buffers, move buffer display
    (let ((buf (current-buffer)))
      (exwm-workspace-switch id)
      (switch-to-buffer buf))))

;;------------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(use-package general
  :demand t
  :after evil
  :config

  ;;------------------------------------------------------------------------------
  ;; Leader key setup
  ;;------------------------------------------------------------------------------
  (general-create-definer leader-key
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;;------------------------------------------------------------------------------
  ;; Main leader bindings
  ;;------------------------------------------------------------------------------
  (leader-key
    ;; Quick actions
    "."  '(find-file :which-key "Find file")
    "q"  '(kill-buffer-and-window :which-key "Kill buffer and window")

    ;; Aidermacs
    "a" '(:ignore t :which-key "Aidermacs")
    "aa" '(aidermacs-transient-menu :which-key "Transient menu")

    ;; Toggle commands
    "t" '(:ignore t :which-key "Toggle")
    "tt"  '(vterm-toggle :which-key "Toggle vterm")

    ;; File commands
    "f"  '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")

    ;; Buffer commands
    "b"  '(:ignore t :which-key "Buffers")
    "bs" '(switch-to-buffer :which-key "Switch buffer")
    "bk" '(kill-buffer :which-key "Kill buffer (select)")
    "bd" '((lambda () (interactive)
             (kill-buffer (current-buffer)))
           :which-key "Kill current buffer")
    "be" '(eval-buffer :which-key "Eval buffer")))

;;------------------------------------------------------------------------------
;; Which-key integration
;;------------------------------------------------------------------------------

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3))


(provide 'custom-keybinds)
;;; custom-keybinds.el ends here
