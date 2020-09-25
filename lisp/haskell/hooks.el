;;; -*- lexical-binding: t; -*-

(after! haskell-mode
  (define-hook! ymacs-haskell|setup (haskell-mode-hook)
    (rainbow-delimiters-mode 1)
    (haskell-decl-scan-mode 1)

    (when (buffer-enable-rich-feature-p)
      (ymacs-lsp//try-enable haskell
        :fallback
        (progn
          (haskell-doc-mode 1)
          (flycheck-mode -1))))

    (haskell-indentation-mode 1)))
