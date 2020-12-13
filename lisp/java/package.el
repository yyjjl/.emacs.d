;;; -*- lexical-binding: t; -*-

(require-packages! lsp-mode lsp-java)

(ymacs-lsp//register-client 'jdtls :package 'lsp-java)
