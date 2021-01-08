;; -*- lexical-binding:t -*-

(defvar calculate-lisp-indent-last-sexp)

(defun ymacs-lisp|common-setup ()
  "Enable features useful in any Lisp mode."
  (setq-local hippie-expand-try-functions-list
              (append hippie-expand-try-functions-list '(try-complete-lisp-symbol)))

  (lispy-mode 1)
  (local-set-key (kbd "M-,") #'xref-pop-marker-stack))

(defun ymacs-lisp|elisp-setup ()
  (ymacs-lisp|common-setup)

  (when (buffer-enable-rich-feature-p)
    (auto-compile-on-save-mode 1)
    (checkdoc-minor-mode)))
