;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-web//web-setup (js-mode-hook js-ts-mode-hook typescript-mode-hook typescript-ts-mode-hook css-mode-hook)
  (electric-indent-local-mode -1)
  (electric-layout-local-mode -1)
  (eval-when-has-feature! lsp
    (when (not (string-suffix-p ".json" (downcase buffer-file-name)))
      (ymacs-lsp//try-enable-eglot web))))
