;;; custom-spell.el --- Spell checking -*- lexical-binding: t; -*-
;;; Commentary:
;; Spell checking configuration

;;; Code:

(use-package ispell
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,es_MX")

  ;; Configure hunspell dictionaries
  (setq ispell-hunspell-dict-paths-alist
        '(("en_US" "/etc/profiles/per-user/mou/share/hunspell/en_US.aff")
          ("es_MX" "/etc/profiles/per-user/mou/share/hunspell/es_MX.aff")
          ("en_US,es_MX" "/etc/profiles/per-user/mou/share/hunspell/en_US.aff")))

  ;; Tell ispell to use the multi-dictionary
  (setq ispell-local-dictionary-alist
        '(("en_US,es_MX" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,es_MX") nil utf-8)))

  (ispell-hunspell-add-multi-dic "en_US,es_MX")

  ;; Set personal dictionary
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (latex-mode . flyspell-mode))

(provide 'custom-spell)
;;; custom-spell.el ends here
