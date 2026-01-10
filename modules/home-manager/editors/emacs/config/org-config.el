;;; org-config.el --- Org Mode Configuration -*- lexical-binding: t; -*-

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "■" "□" "▼" "▽" "▷" "◁"))
  (org-modern-block-name nil)
  (org-modern-keyword nil))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autostars t)
  (org-appear-inside-latex t))

(use-package org
  :ensure nil
  :hook (org-mode . (lambda () (org-indent-mode)))
  :custom
  (org-hide-emphasis-markers t)
  (org-ellipsis "…")
  (org-confirm-babel-evaluate nil))

;; Org Roam: Knowledge Base
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Org/Roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you want org-roam-ui later, it goes here
  (org-roam-setup))

;; Org Download: Drag and drop images
(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :config
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_"))

;; Ob-lean4
(use-package ob-lean4
  :load-path "/home/mou/Desktop/Personal/dotfiles/ob-lean4"
  :ensure nil
  :demand t)

;; env-rc
(use-package envrc
:ensure nil
:config
(envrc-global-mode))

(defun my/org-babel-tramp-inject-path (params)
(let ((dir (alist-get :dir params)))
  (if (and dir (string-prefix-p "/sudo::" dir))
      (cons (cons :prologue (format "export PATH='%s'" (mapconcat 'identity exec-path ":")))
      params)
params)))

(advice-add 'org-babel-get-src-block-info :filter-return #'my/org-babel-tramp-inject-path)

;; ob-async
(use-package ob-async
:ensure nil
(require 'ob-async))

(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (lean4 . t)
     (restclient . t)))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all)
  (yas-global-mode 1))

(provide 'org-config)