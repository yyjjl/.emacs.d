;;; -*- lexical-binding: t; -*-

(after! cc-mode
  (define-hook! ymacs-java|setup (java-mode-hook)
    (when (buffer-enable-rich-feature-p)
      (require 'lsp-java)

      (setq-local c-basic-offset 8)
      (try-enable-lsp! java
        :-init
        (setq ymacs-lsp-format-buffer-function #'lsp-java-organize-imports)))))
