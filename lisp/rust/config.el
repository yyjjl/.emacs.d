;;; -*- lexical-binding: t; -*-

(after! rust-mode
  (define-key! :map rust-mode-map
    ("C-c C-f")
    ([remap delete-char] . c-hungry-delete-forward)
    ([remap delete-backward-char] . c-hungry-delete-backwards)

    ("C-c b" . rust-format-buffer)
    ("C-c C-b" . rust-format-buffer)

    (:has-feature lsp
     ("C-c b" lsp-format-buffer)
     ("C-c C-b" lsp-format-buffer)))

  (require 'cc-mode))

(after! cargo
  (define-key! :map cargo-minor-mode-map
    ("C-c C-c" . ymacs-rust/cargo-dispatch)
    ("C-c C-l" . ymacs-rust/cargo-run))

  (setq cargo-process--command-flags "--color never"))
