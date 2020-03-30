;;; -*- lexical-binding: t; -*-

(require-packages! lsp-mode lsp-java)

(config! cc-mode
  :bind
  (:map java-mode-map
   ([f5] . dap-hydra)
   ([f10] . lsp-java-build-project)
   ("C-c b" . lsp-format-buffer)
   ("C-c C-b" . lsp-format-buffer)
   ("C-c B" . lsp-java-organize-imports)
   ("C-c C-d" . lsp-describe-thing-at-point))

  :hook
  (setup
   :define (java-mode-hook)
   (when (buffer-enable-rich-feature-p)
     (require 'lsp-java)

     (setq-local c-basic-offset 8)
     (lsp//try-enable java|setup))))

(config! lsp-java
  :config
  (setq lsp-java-server-install-dir (expand-var! "eclipse.jdt.ls/server/"))
  (setq lsp-java-workspace-dir (expand-var! "lsp-java"))
  (setq lsp-java-workspace-cache-dir (expand-var! "lsp-java/.cache"))
  (setq lsp-java-save-action-organize-imports nil))

(provide 'init-java)
