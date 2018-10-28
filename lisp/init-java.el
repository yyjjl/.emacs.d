;;; -*- lexical-binding: t; -*-

(require-packages!
 lsp-mode
 lsp-java
 dap-mode
 company-lsp)



(define-hook! java|setup (java-mode-hook)
  (unless (or (buffer-temporary-p)
              (file-remote-p default-directory)
              (> (buffer-size) core-large-buffer-size))
    (with-demoted-errors "Lsp-java: %s"
      (lsp-java-enable))))

(with-eval-after-load 'lsp-java
  (setq lsp-java-server-install-dir (expand-var! "eclipse.jdt.ls/server/"))
  (setq lsp-java-workspace-dir (expand-var! "lsp-java"))
  (setq lsp-java-workspace-cache-dir (expand-var! "lsp-java/.cache"))

  (define-key! :map java-mode-map
    ([f5] . dap-hydra)
    ([C-f5] . dap-java-debug)
    ([f10] . lsp-java-build-project)
    ("C-c b" . lsp-format-buffer)
    ("C-c C-d" . lsp-describe-thing-at-point))

  (dap-mode 1)
  (dap-ui-mode 1))

(provide 'init-java)
