;;; -*- lexical-binding: t; -*-

(after! rust-mode
  (define-hook! ymacs-rust|setup (rust-mode-hook)
    (cargo-minor-mode 1)
    (when (buffer-enable-rich-feature-p)
      (ymacs-lsp//try-enable rust))))
