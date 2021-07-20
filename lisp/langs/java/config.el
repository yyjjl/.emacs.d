;;; -*- lexical-binding: t; -*-

(eval-when-has-feature! lsp
  (after! cc-mode
    (define-key! :map java-mode-map
      ("C-c C-d")
      ("C-c t" . lsp-java-type-hierarchy)
      ("C-c o" . lsp-java-open-super-implementation)
      ([f9] . lsp-java-build-project)))

  (after! lsp-java
    (setq lsp-java-references-code-lens-enabled t)
    (setq lsp-java-implementations-code-lens-enabled t)
    (setq lsp-java-autobuild-enabled nil)split-width-threshold

    (setq lsp-java-server-install-dir (expand-cache! "lsp/eclipse.jdt.ls/"))
    (setq lsp-java-workspace-dir (expand-cache! "lsp-java"))
    (setq lsp-java-workspace-cache-dir (expand-cache! "lsp-java/.cache/"))
    (setq lsp-java-save-action-organize-imports nil)))
