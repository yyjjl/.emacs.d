;;; -*- lexical-binding: t; -*-

(after! haskell-mode
  (define-hook! ymacs-haskell//setup (haskell-mode-hook haskell-literate-mode-hook)
    (haskell-decl-scan-mode 1)
    (haskell-indentation-mode 1)

    (eval-when-has-feature! lsp
      (with-transient-hook! (hack-local-variables-hook :local t)
        (unless (and (is-buffer-suitable-for-coding!)
                     (ymacs-lsp//try-enable haskell))
          (haskell-doc-mode 1))))))
