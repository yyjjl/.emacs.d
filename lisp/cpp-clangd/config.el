;; -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-disabled-clients '(ccls))
  (setq lsp-clients-clangd-executable ymacs-clangd-path)
  (setq lsp-clients-clangd-args
        '("--all-scopes-completion"
          "--clang-tidy"
          "--suggest-missing-includes")))
