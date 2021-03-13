;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (after! go-mode
    (define-hook! ymacs-go//setup (go-mode-hook)
      (ymacs-lsp//try-enable-simple go))))
