;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-web//web-setup (web-mode-hook js-mode-hook typescript-mode-hook css-mode-hook)
  (electric-indent-local-mode -1)
  (electric-layout-local-mode -1)
  (eval-when-has-feature! lsp
    (ymacs-lsp//try-enable-simple web
      (not (string-suffix-p ".json" (downcase buffer-file-name)))))

  (when (eq major-mode web-mode-hook)
    (when (equal web-mode-content-type "jsx")
      (setq-local web-mode-enable-auto-quoting nil))))
