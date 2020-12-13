;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-sh|setup (sh-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (try-enable-lsp! sh)))

(add-hook 'makefile-mode-hook #'whitespace-mode)
