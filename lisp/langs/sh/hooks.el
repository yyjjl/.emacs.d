;;; -*- lexical-binding: t; -*-

(after! sh-script
  (define-hook! ymacs-sh//setup (sh-mode-hook)
    (when (is-buffer-suitable-for-coding!)
      (try-enable-lsp! sh))))

(after! make-mode
  (add-hook 'makefile-mode-hook #'whitespace-mode))
