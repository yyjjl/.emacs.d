;;; -*- lexical-binding: t; -*-

(defun ymacs-python//generate-doc (-params -indent)
  (setq -indent (concat "\n" -indent))
  (string-join (mapcar (lambda (token)
                         (let ((param (split-string token "=" t " +"))
                               default)
                           (setq default (cadr param))
                           (setq param (car param))
                           (concat "@param " param
                                   (if default
                                       (format " (default: %s): " default)
                                     ": ")
                                   -indent "@type " param ": ")))
                       (split-string -params "," t " +"))
               -indent))

(defun ymacs-python//shell-running-p ()
  (when-let* ((process (or (and (derived-mode-p 'inferior-python-mode)
                                (get-buffer-process (current-buffer)))
                           (python-shell-get-process))))
    (with-current-buffer (process-buffer process)
      (not comint-last-prompt))))

(defun ymacs-python//elpy-enable ()
  (require 'elpy)
  (unless (bound-and-true-p elpy-enabled-p)
    (setq elpy-enabled-p t)
    (elpy-modules-global-init)
    (add-hook 'pyvenv-post-activate-hooks 'elpy-rpc--disconnect)
    (add-hook 'pyvenv-post-deactivate-hooks 'elpy-rpc--disconnect)
    (add-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter)
    (add-hook 'python-shell-first-prompt-hook 'elpy-shell--send-setup-code t)))
