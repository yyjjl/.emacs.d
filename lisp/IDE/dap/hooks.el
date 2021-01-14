;; -*- lexical-binding:t -*-

(after! dap-mode
  (define-advice dap-debug (:before (&rest _args) save-window-configuration)
    (ymacs-debug//before-debug))

  ;; Activate this minor mode when dap is initialized
  (define-hook! (ymacs-dap|stopped _session) (dap-stopped-hook)
    (unless ymacs-dap-running-session-mode
      (ymacs-dap-running-session-mode 1)))

  (define-hook! (ymacs-dap|terminated _session) (dap-terminated-hook)
    (ymacs-debug//after-debug #'ymacs-dap-running-session-mode)
    (dap-hydra/nil))

  (define-hook! (ymacs-dap|stack-frame-changed -session) (dap-stack-frame-changed-hook)
    (when (and (dap--session-running -session)
               (not ymacs-dap-running-session-mode))
      (ymacs-dap-running-session-mode 1))))
