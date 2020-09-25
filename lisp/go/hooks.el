;;; -*- lexical-binding: t; -*-

(after! go-mode
  (define-hook! ymacs-go|setup (go-mode-hook)
    (when (buffer-enable-rich-feature-p)
      (ymacs-lsp//try-enable go))))
