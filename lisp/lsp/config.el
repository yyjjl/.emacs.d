;; -*- lexical-binding:t -*-

(setq lsp-keymap-prefix nil)
(after! lsp-mode
  (define-key! :map lsp-mode-map
    ("M-s l" . lsp-lens-mode)
    ("M-s h h" . lsp-document-highlight)
    ("M-s '" . lsp-avy-lens)
    ("M-s d" . dap-hydra)
    ("C-c R" . lsp-rename)
    ("C-c I" . lsp-ivy-workspace-symbol)
    ("C-c G" . lsp-ivy-global-workspace-symbol)
    ("C-c S" . lsp-describe-session)
    ("C-c B" . lsp-format-buffer)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)

    ("M-o" . lsp-signature-activate)
    ("C-S-SPC" . lsp-signature-activate))

  (define-key! :map lsp-signature-mode-map
    ("M-o" . lsp-signature-stop))

  (setq lsp-auto-configure t)
  ;; (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-diagnostics-provider :auto)
  ;; (setq lsp-restart 'auto-restart)
  (setq lsp-session-file (expand-var! "lsp-sessions"))
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-signature-doc-lines 2)
  (setq lsp-signature-auto-activate '(:on-trigger-char))
  (setq lsp-idle-delay 0.5)

  (setq lsp-completion-provider :none)

  (setq lsp-enable-snippet t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-semantic-highlighting nil)

  (setq lsp-signature-doc-lines 2)

  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t))
