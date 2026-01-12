;;; ui.el --- General UI/UX -*- lexical-binding: t; -*-

;; Font
(defun mou/setup-fonts ()
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110)
  (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 110)
  (set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 110))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (mou/setup-fonts))))
  (mou/setup-fonts))

;; Rainbow Mode: Colorize color names/codes in buffers
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Centaur Tabs: Modern, aesthetic tabs
(use-package centaur-tabs
  :demand
  :init
  ;; 1. Set default to NIL (Safe for daemon startup)
  (setq centaur-tabs-set-icons nil)

  ;; 2. Define function to enable icons ONLY when a GUI frame exists
  (defun mou/centaur-tabs-gui-setup (frame)
    (with-selected-frame frame
      (when (display-graphic-p)
	(setq centaur-tabs-set-icons t) ;; Enable icons now that we have a GUI
	(centaur-tabs-mode 1))))

  ;; 3. Hook it to run when a client connects
  ;; (add-hook 'after-make-frame-functions #'mou/centaur-tabs-gui-setup)

  :config
  ;; 4. If running standalone (Normal Emacs), run setup immediately
  ;; (unless (daemonp)
  ;;   (mou/centaur-tabs-gui-setup (selected-frame)))

  (defun centaur-tabs-buffer-groups ()
    "Group buffers into 'Special' and 'Tabs'."
    (list
     (cond
      ((or (and (stringp (buffer-name)) 
                (> (length (buffer-name)) 0)
                (string-equal "*" (substring (buffer-name) 0 1)))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      magit-revision-mode
			      helpful-mode
			      dashboard-mode)))
       "Special")
      (t "Tabs"))))
  
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  ;; (centaur-tabs-set-icons t)  <-- DELETED from here. It is handled in :init now.
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'left)
  (centaur-tabs-cycle-scope 'tabs)
  
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
    (define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)))

;; Clean up the UI
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font (Optional - Adjust to your system)
;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

;; Dashboard: A fancy startup screen
(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "/home/mou/.emacs.d/donut.txt")
  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)
			  (bookmarks . 5)))
  (setq dashboard-banner-logo-title "Welcome back, Mou"))

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Dirvish: Modern Dired replacement
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
	'(:left (sort section file-time " " file-size symlink) :right (omit yank index)))
  (setq dirvish-attributes
	'(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  :bind
  (("C-x d" . dirvish)
   :map dirvish-mode-map
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; nerd-icons-dired is usually superseded by dirvish's own icon support,
;; but keeping it doesn't hurt if we disable it or if dirvish is toggled off.
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps t)
  (setq which-key-description-replacement-alist
	'(("evil-\\(.*\\)" . "\\1")
	  ("my/\\(.*\\)" . "\\1"))))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Dimmer: Focus the active window
;; (use-package dimmer
;;   :config
;;   (setq dimmer-fraction 0.2)
;;   (dimmer-configure-which-key)
;;   (dimmer-configure-helm)
;;   (dimmer-mode t))

;; Diff-hl: Git gutter
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . ignore)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(provide 'ui)
