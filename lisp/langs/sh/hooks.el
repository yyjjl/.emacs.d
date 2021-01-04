;;; -*- lexical-binding: t; -*-

(after! sh-script
  (define-hook! ymacs-sh|setup (sh-mode-hook)
    (when (buffer-enable-rich-feature-p)
      (try-enable-lsp! sh))))

(after! make-mode
  (add-hook 'makefile-mode-hook #'whitespace-mode))
