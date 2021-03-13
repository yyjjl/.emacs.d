;;; -*- lexical-binding: t; -*-

(after! cc-mode
  (define-hook! ymacs-java//setup (java-mode-hook)
    (setq-local c-basic-offset 8)

    (eval-when-has-feature! lsp
      (require 'lsp-java)

      (with-transient-hook! (hack-local-variables-hook :local t)
        (when (and (is-buffer-suitable-for-coding!)
                   (ymacs-lsp//try-enable java))
          (setq ymacs-lsp-organize-import-function #'lsp-java-organize-imports))))))
