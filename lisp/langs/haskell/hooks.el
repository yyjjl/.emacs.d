;;; -*- lexical-binding: t; -*-

(after! haskell-mode
  (define-hook! ymacs-haskell//setup (haskell-mode-hook haskell-literate-mode-hook)
    (haskell-decl-scan-mode 1)
    (haskell-indentation-mode 1)
    (haskell-doc-mode 1)

    (with-transient-hook! (hack-local-variables-hook :local t)
      (when (is-buffer-suitable-for-coding!)
        (eval-when-has-feature! lsp
          (ymacs-lsp//try-enable-eglot haskell
            (haskell-doc-mode -1)))))))
