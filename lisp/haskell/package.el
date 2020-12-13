;;; -*- lexical-binding: t; -*-

(require-packages!
 rainbow-delimiters
 haskell-mode
 lsp-haskell)

(autoload 'haskell-debug "haskell-debug" nil t)

(ymacs-lsp//register-client 'lsp-haskell :package 'lsp-haskell)

