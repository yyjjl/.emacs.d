;;; -*- lexical-binding: t; -*-

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

