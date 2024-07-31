;;; -*- lexical-binding: t; -*-

(ymacs-editor//set-font-lock-level :modes (java-mode) :level 2)

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(java-mode jdb :gud t)))

(eval-when-has-feature! lsp
  (require-packages! eglot-java))
