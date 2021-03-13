;;; -*- lexical-binding: t; -*-

(after! rust-mode
  (define-hook! ymacs-rust//setup (rust-mode-hook)
    (cargo-minor-mode 1)

    (eval-when-has-feature! lsp
      (ymacs-lsp//try-enable-simple rust))))
