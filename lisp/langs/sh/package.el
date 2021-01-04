;; -*- lexical-binding:t -*-

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'bash-ls :package 'lsp-bash))
