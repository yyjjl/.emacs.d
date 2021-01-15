;;; -*- lexical-binding: t; -*-

(after! go-mode
  (define-hook! ymacs-go//setup (go-mode-hook)
    (when (is-buffer-suitable-for-coding!)
      (try-enable-lsp! go))))
