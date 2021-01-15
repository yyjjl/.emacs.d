;;; -*- lexical-binding: t; -*-

(after! rust-mode
  (define-hook! ymacs-rust//setup (rust-mode-hook)
    (cargo-minor-mode 1)
    (when (is-buffer-suitable-for-coding!)
      (try-enable-lsp! rust))))
