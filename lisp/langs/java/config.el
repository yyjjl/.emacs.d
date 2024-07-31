;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (setq eglot-java-server-install-dir (expand-cache! "lsp/eclipse.jdt.ls")))
