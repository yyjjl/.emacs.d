;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (require-packages! lsp-java)

  (ymacs-lsp//register-client 'jdtls :package 'lsp-java))
