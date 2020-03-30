;;; -*- lexical-binding: t; -*-

(require-packages! go-mode go-tag)

(config! go-mode
  :bind
  (:map go-mode-map
   ("C-c t t" . go-tag-add)
   ("C-c t r" . go-tag-refresh)
   ("C-c t R" . go-tag-remove))

  :hook
  (setup
   :define (go-mode-hook)
   (when (buffer-enable-rich-feature-p)
     (lsp//try-enable go|setup))))

(provide 'init-go)
