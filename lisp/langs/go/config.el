;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (after! lsp-go
    (ymacs-lsp//set-simple-install-fn
     'gopls
     (list "bash" (expand-etc! "scripts/install_gopls")))))

(after! go-mode
  (define-key! :map go-mode-map
    ("C-c t t" . go-tag-add)
    ("C-c t r" . go-tag-refresh)
    ("C-c t R" . go-tag-remove)))

(after! go-ts-mode
  (define-key! :map go-ts-mode-map
    ("C-c t t" . go-tag-add)
    ("C-c t r" . go-tag-refresh)
    ("C-c t R" . go-tag-remove)))

