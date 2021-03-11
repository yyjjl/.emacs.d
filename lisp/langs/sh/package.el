;; -*- lexical-binding:t -*-

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'bash-ls :package 'lsp-bash))

(after! sh-script
  (define-hook! ymacs-sh//setup (sh-mode-hook)
    (when (is-buffer-suitable-for-coding!)
      (try-enable-lsp! sh))))
