;;; -*- lexical-binding: t; -*-

(require-packages!
 lsp-mode
 lsp-java
 dap-mode
 company-lsp)



(defcustom java-setup-use-lsp nil
  "Whether to setup project literally"
  :group 'lsp-java
  :type 'directory
  :safe #'booleanp)

(define-hook! java|setup (java-mode-hook)
  (unless (or (buffer-temporary-p)
              (file-remote-p default-directory)
              (> (buffer-size) core-large-buffer-size))
    (add-transient-hook!
        (hack-local-variables-hook :local t :name java//setup-interal)
      (when java-setup-use-lsp
        (with-demoted-errors "Lsp-java: %s"
          (lsp)
          (lsp-ui-doc-mode 1))))))

(with-eval-after-load 'cc-mode
  (require 'lsp-java))

(with-eval-after-load 'lsp-java
  (setq lsp-java-server-install-dir (expand-var! "eclipse.jdt.ls/server/"))
  (setq lsp-java-workspace-dir (expand-var! "lsp-java"))
  (setq lsp-java-workspace-cache-dir (expand-var! "lsp-java/.cache"))
  (setq lsp-java-save-action-organize-imports nil)

  (define-key! :map java-mode-map
    ([f5] . dap-hydra)
    ([C-f5] . dap-java-debug)
    ([f10] . lsp-java-build-project)
    ("C-c b" . lsp-format-buffer)
    ("C-c B" . lsp-java-organize-imports)
    ("C-c C-d" . lsp-describe-thing-at-point))

  (dap-mode 1)
  (dap-ui-mode 1))

(provide 'init-java)
