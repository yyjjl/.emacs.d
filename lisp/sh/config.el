;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-sh|setup (sh-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (ymacs-lsp//try-enable sh|setup
      :init (setq-local lsp-eldoc-render-all nil))))

(add-hook 'makefile-mode-hook #'whitespace-mode)
