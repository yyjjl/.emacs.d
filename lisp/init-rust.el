;;; -*- lexical-binding: t; -*-

(require-packages!
 cargo
 ;; Emacs 26 has conf-toml-mode
 (toml-mode :when (<= emacs-major-version 25))
 rust-mode)

(defvar rust-cargo-commands
  (eval-when-compile
    (let (commands)
      (mapatoms (lambda (symbol)
                  (let ((name (symbol-name symbol)))
                    (when (and (string-prefix-p "cargo-process" name)
                               (commandp symbol)
                               (not (memq symbol '(cargo-process-mode
                                                   cargo-process-run))))
                      (push (cons (string-join (cddr (split-string name "-")) "-")
                                  symbol)
                            commands)))))
      commands)))

(define-hook! rust|setup (rust-mode-hook)
  (cargo-minor-mode 1)
  (when (buffer-enable-rich-feature-p)
    (lsp//try-enable rust|setup-internal)))

(with-eval-after-load 'rust-mode
  (require 'cc-mode nil t)

  (setq rust-cargo-bin "~/.cargo/bin/cargo")
  (define-key! :map rust-mode-map
    ("C-c C-f")
    ([remap delete-char] . c-hungry-delete-forward)
    ([remap delete-backward-char] . c-hungry-delete-backwards)
    ("C-c b" . rust-format-buffer)
    ("C-c C-b" . rust-format-buffer)))

(with-eval-after-load 'cargo
  (setq cargo-process--custom-path-to-bin "~/.cargo/bin/cargo"
        cargo-process--command-flags "--color never")

  (define-key! :map cargo-minor-mode-map
    ("C-c C-c" . rust/cargo-dispatch)
    ("C-c C-l" . rust/cargo-run)))

(provide 'init-rust)
