;;; -*- lexical-binding: t; -*-

(require-packages! go-mode go-tag)

(with-eval-after-load 'go-mode
  (define-key! :map go-mode-map
    ("C-c t t" . go-tag-add)
    ("C-c t r" . go-tag-refresh)
    ("C-c t R" . go-tag-remove)))

(define-hook! go|setup (go-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (lsp//try-enable go|setup-internal)))

(provide 'init-go)
