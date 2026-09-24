;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! IDE/lsp
  (setq eglot-java-server-install-dir (expand-cache! "lsp/eclipse.jdt.ls")))
