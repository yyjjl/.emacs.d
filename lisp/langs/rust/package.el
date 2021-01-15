;;; -*- lexical-binding: t; -*-

(require-packages! cargo rust-mode)

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'rust-analyzer :package 'lsp-rust))

(defvar ymacs-rust-cargo-commands
  (eval-when-compile
    (let (commands)
      (mapatoms (lambda (symbol)
                  (let ((name (symbol-name symbol)))
                    (when (and (string-prefix-p "cargo-process" name)
                               (commandp symbol)
                               (not (memq symbol '(cargo-process-mode cargo-process-run))))
                      (push (cons (string-join (cddr (split-string name "-")) "-")
                                  symbol)
                            commands)))))
      commands)))
