;;; -*- lexical-binding: t; -*-

(require-packages!
 rainbow-delimiters
 haskell-mode)

(eval-when-has-feature! lsp
  (require-packages! lsp-haskell)

  (ymacs-lsp//register-client 'lsp-haskell :package 'lsp-haskell))

(autoload 'haskell-debug "haskell-debug" nil t)

