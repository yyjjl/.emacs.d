;;; -*- lexical-binding: t; -*-

(after! haskell-mode
  (define-hook! ymacs-haskell|setup (haskell-mode-hook haskell-literate-mode-hook)
    (haskell-decl-scan-mode 1)
    (rainbow-delimiters-mode 1)

    (when (buffer-enable-rich-feature-p)
      (try-enable-lsp! haskell
        :fallback
        (progn
          (haskell-doc-mode 1)
          (flycheck-mode -1))))

    (haskell-indentation-mode 1)))
