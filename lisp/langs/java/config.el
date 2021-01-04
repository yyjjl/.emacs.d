;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (after! cc-mode
    (define-key! :map java-mode-map
      ([f5] . dap-hydra)
      ([f10] . lsp-java-build-project)
      ("C-c C-d" . lsp-describe-thing-at-point)))

  (after! lsp-java
    (setq lsp-java-server-install-dir (expand-cache! "lsp/eclipse.jdt.ls/server/"))
    (setq lsp-java-workspace-dir (expand-cache! "lsp-java"))
    (setq lsp-java-workspace-cache-dir (expand-cache! "lsp-java/.cache"))
    (setq lsp-java-save-action-organize-imports nil)))
