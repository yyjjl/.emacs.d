;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (define-hook! ymacs-go//setup (go-mode-hook go-ts-mode-hook)
    (ymacs-lsp//try-enable-simple go)))
