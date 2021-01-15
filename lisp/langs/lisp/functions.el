;; -*- lexical-binding:t -*-

(defun ymacs-lisp//common-setup ()
  "Enable features useful in any Lisp mode."
  (setq-local hippie-expand-try-functions-list
              (append hippie-expand-try-functions-list '(try-complete-lisp-symbol)))

  (lispy-mode 1))

(defun ymacs-lisp//elisp-setup ()
  (ymacs-lisp//common-setup)

  (when (is-buffer-suitable-for-coding!)
    (auto-compile-on-save-mode 1)
    (checkdoc-minor-mode 1)))
