;;; custom-spell.el --- Spell checking -*- lexical-binding: t; -*-
;;; Commentary:
;; Spell checking configuration

;;; Code:

(use-package ispell
  :config
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US")

    ;; Set personal dictionary
    (setq ispell-personal-dictionary "~/.hunspell_personal")

    ;; Let hunspell find dictionaries automatically through DICPATH
    ;; Hunspell will search standard locations including:
    ;; - /etc/profiles/per-user/$USER/share/hunspell
    ;; - ~/.nix-profile/share/hunspell
    ;; - System paths
    (setenv "DICPATH" (string-join
                       '("~/.nix-profile/share/hunspell"
                         "/etc/profiles/per-user/mou/share/hunspell"
                         "/run/current-system/sw/share/hunspell")
                       ":"))))

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (latex-mode . flyspell-mode))

(provide 'custom-spell)
;;; custom-spell.el ends here
