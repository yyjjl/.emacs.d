;; -*- lexical-binding:t -*-

(after! dap-mode

  ;; Activate this minor mode when dap is initialized
  (define-hook! (ymacs-lsp|dap-session-created _session) (dap-session-created-hook)
    (unless ymacs-lsp/dap-running-session-mode
      (ymacs-lsp/dap-running-session-mode 1))
    (dap-hydra))

  (define-hook! (ymacs-lsp|dap-stopped _session) (dap-stopped-hook)
    (unless ymacs-lsp/dap-running-session-mode
      (ymacs-lsp/dap-running-session-mode 1))
    (dap-hydra))

  (define-hook! (ymacs-lsp|dap-terminated _session) (dap-terminated-hook)
    (ymacs-lsp/dap-running-session-mode -1)
    (dap-hydra/nil))

  (define-hook! (ymacs-lsp|dap-stack-frame-changed -session) (dap-stack-frame-changed-hook)
    (when (and (dap--session-running -session)
               (not ymacs-lsp/dap-running-session-mode))
      (ymacs-lsp/dap-running-session-mode 1))))
