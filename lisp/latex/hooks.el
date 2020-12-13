;;; -*- lexical-binding: t; -*-

(after! latex
  (define-hook! ymacs-latex|setup (LaTeX-mode-hook)
    (ymacs-latex//common-setup)

    (when (buffer-enable-rich-feature-p)
      (ymacs-lsp//try-enable latex))))
