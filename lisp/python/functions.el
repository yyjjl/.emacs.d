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
