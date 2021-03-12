;;; -*- lexical-binding: t; -*-

(require-packages! haskell-mode)

(ymacs-editor//set-input-method
 :mode haskell-mode
 :ime haskell-unicode-input-method
 :package haskell-unicode)

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(haskell-mode haskell-debug)))

(eval-when-has-feature! lsp
  (require-packages! lsp-haskell)

  (ymacs-lsp//register-client 'lsp-haskell :package 'lsp-haskell))

(autoload 'haskell-debug "haskell-debug" nil t)
(autoload #'haskell-indent-put-region-in-literate "haskell-indent" nil t)
(autoload #'haskell-indent-insert-guard "haskell-indent" nil t)

