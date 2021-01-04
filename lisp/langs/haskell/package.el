;;; -*- lexical-binding: t; -*-

(require-packages! haskell-mode)

(add-to-list
 'ymacs-default-input-method-alist
 '(haskell-mode haskell-unicode-input-method haskll-unicode))

(eval-when-has-feature! lsp
  (require-packages! lsp-haskell)

  (ymacs-lsp//register-client 'lsp-haskell :package 'lsp-haskell))

(autoload 'haskell-debug "haskell-debug" nil t)
(autoload #'haskell-indent-put-region-in-literate "haskell-indent" nil t)
(autoload #'haskell-indent-insert-guard "haskell-indent" nil t)

