;;; ob-idris.el --- Org-babel functions for Idris 2 evaluation -*- lexical-binding: t; -*-

(require 'ob)
(require 'comint)

(defvar org-babel-default-header-args:idris
  '((:results . "output") (:exports . "both")))

;; Regex to match the prompt: Start of line, some chars, then > and space.
(defvar org-babel-idris-prompt "^[ \t]*[a-zA-Z0-9._]*> *"
  "Regexp to match the Idris 2 prompt.")

(defun org-babel-idris-clean-output (output)
  "Clean up Idris 2 REPL output."
  (let ((clean output))
    ;; Remove the command that was echoed (if any)
    ;; This is heuristic; removing the first line if it looks like the command might be needed
    ;; But for now, just strip prompts.
    (setq clean (replace-regexp-in-string org-babel-idris-prompt "" clean))
    (setq clean (replace-regexp-in-string "Bye for now!" "" clean))
    (string-trim clean)))

(defun org-babel-prep-session:idris (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (let* ((session (if (and session (not (string= session "none")))
                      session
                    "idris"))
         (buf-name (if (string-prefix-p "*" session)
                       session
                     (concat "*" session "*")))
         (buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (make-comint-in-buffer session buf-name "idris2" nil "--no-color" "--no-banner" "--quiet"))
      (with-current-buffer buffer
        (setq-local comint-prompt-regexp org-babel-idris-prompt)
        (setq-local comint-process-echoes t)
        ;; Wait for the initial prompt
        (let ((found nil))
          (dotimes (_ 20) ;; Try for 2 seconds (20 * 0.1)
            (unless found
              (sleep-for 0.1)
              (goto-char (point-max))
              (when (re-search-backward org-babel-idris-prompt nil t)
                (setq found t)))))))
    buffer))

(defun org-babel-execute:idris (body params)
  "Execute a block of Idris 2 code with org-babel."
  (let* ((session (cdr (assq :session params)))
         (has-main (string-match-p "^[ \t]*main[ \t]*:" body)))
    (if (and session (not (string= session "none")))
        ;; SESSION MODE
        (let* ((session-buf (org-babel-prep-session:idris session params))
               (is-module (or (string-match-p "^[ \t]*module " body)
                              (string-match-p "^[ \t]*[a-zA-Z0-9_']+[ \t]*:" body))))
          (org-babel-idris-clean-output
           (org-babel-comint-with-output
               (session-buf org-babel-idris-prompt)
             (with-current-buffer session-buf
               (goto-char (point-max))
               (if is-module
                   (let ((temp-file (org-babel-temp-file "idris-" ".idr")))
                     (with-temp-file temp-file (insert body))
                     (insert (format ":l \"%s\"" temp-file))
                     (comint-send-input nil t))
                 (dolist (line (split-string body "\n"))
                   (unless (string-empty-p (string-trim line))
                     (insert line)
                     (comint-send-input nil t)
                     ;; Small sleep to keep order if multiple lines
                     (sleep-for 0.01))))))))
      ;; NO SESSION MODE
      (if has-main
          ;; EXEC MODE
          (let* ((temp-file (org-babel-temp-file "idris-" ".idr"))
                 (temp-dir (file-name-directory temp-file))
                 (temp-filename (file-name-nondirectory temp-file))
                 (cmd (format "cd %s && idris2 --no-color %s --exec main"
                              (org-babel-process-file-name temp-dir)
                              (org-babel-process-file-name temp-filename))))
            (with-temp-file temp-file
              (unless (string-match-p "^module " body)
                (insert "module Main\n\n"))
              (insert body))
            (org-babel-eval cmd ""))
        ;; REPL MODE (One-shot)
        (org-babel-idris-clean-output
         (org-babel-eval "idris2 --no-color --no-banner --quiet" body))))))

(provide 'ob-idris)

