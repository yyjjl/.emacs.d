;;; -*- lexical-binding: t; -*-

(define-hook! ymacs-java//setup (java-mode-hook java-ts-mode-hook)
  (setq-local c-basic-offset 4)
  (setq-local java-ts-mode-indent-offset 4)

  (eval-when-has-feature! lsp
    (with-transient-hook! (hack-local-variables-hook :local t)
      (when (is-buffer-suitable-for-coding!)
        (ymacs-lsp//try-enable-eglot java)))))
