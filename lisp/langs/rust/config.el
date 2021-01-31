;;; -*- lexical-binding: t; -*-

(after! rust-mode
  (define-key! :map rust-mode-map
    ("C-c C-f")
    ([remap delete-char] . c-hungry-delete-forward)
    ([remap delete-backward-char] . c-hungry-delete-backwards)
    ("C-c C-b" . rust-format-buffer))

  (require 'cc-mode))

(after! cargo
  (define-key! :map cargo-mode-map
    ("C-c C-c" . ymacs-rust/cargo-dispatch)))
