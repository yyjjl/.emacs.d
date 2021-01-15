;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(java-mode jdb :gud t)))

(eval-when-has-feature! lsp
  (require-packages! lsp-java)

  (ymacs-lsp//register-client 'jdtls :package 'lsp-java))
