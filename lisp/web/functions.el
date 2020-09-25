;;; -*- lexical-binding: t; -*-

(defun ymacs-web//imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

(defun ymacs-web|common-setup ()
  (when (bound-and-true-p lsp-enable-in-project-p)
    (electric-indent-local-mode -1)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (flycheck-mode 1)
    (tide-setup)
    (tide-hl-identifier-mode 1)

    (ymacs-company//add-backend 'company-tide)))
