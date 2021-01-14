;; -*- lexical-binding:t -*-

(define-minor-mode ymacs-dap-running-session-mode
  "A mode for adding keybindings to running sessions"
  nil nil
  ymacs-dap-running-session-mode-map
  (if ymacs-dap-running-session-mode
      (ymacs-debug//enable)
    (ymacs-debug//disable))

  (force-mode-line-update))

