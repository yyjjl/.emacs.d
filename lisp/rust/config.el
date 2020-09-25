;;; -*- lexical-binding: t; -*-

(after! rust-mode
  (define-key! :map rust-mode-map
    ("C-c C-f")
    ([remap delete-char] . c-hungry-delete-forward)
    ([remap delete-backward-char] . c-hungry-delete-backwards)
    ("C-c b" . rust-format-buffer)
    ("C-c C-b" . rust-format-buffer))

  (require 'cc-mode))

(after! cargo
  (define-key! :map cargo-minor-mode-map
    ("C-c C-c" . ymacs-rust/cargo-dispatch)
    ("C-c C-l" . ymacs-rust/cargo-run))

  (setq cargo-process--command-flags "--color never"))

(after! lsp-rust
  (let ((url "https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux")
        (path (expand-var! "rust-analyzer")))
    (setq lsp-rust-analyzer-server-command (list path))

    (ymacs-lsp//set-simple-install-fn
     'rust-analyzer
     (format "curl -L %s -o %s && chmod +x %s" url path path))))
