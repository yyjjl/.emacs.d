;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-java//setup (java-mode-hook java-ts-mode-hook)
  (setq-local c-basic-offset 4)
  (setq-local java-ts-mode-indent-offset 4)

  (eval-when-has-feature! lsp
    (require 'lsp-java)

    (with-transient-hook! (hack-local-variables-hook :local t)
      (when (and (is-buffer-suitable-for-coding!)
                 (ymacs-lsp//try-enable java))
        (setq ymacs-lsp-organize-import-function #'lsp-java-organize-imports)))))
