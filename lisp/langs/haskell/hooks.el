;;; -*- lexical-binding: t; -*-

(after! haskell-mode
  (define-hook! ymacs-haskell//setup (haskell-mode-hook haskell-literate-mode-hook)
    (haskell-decl-scan-mode 1)

    (when (is-buffer-suitable-for-coding!)
      (try-enable-lsp! haskell
        :-fallback
        (haskell-doc-mode 1)))

    (haskell-indentation-mode 1)))
