;; -*- lexical-binding:t -*-

(after! dap-hydra
  (defhydra++ dap-hydra ()
    ("q" :delete)
    ("ed" dap-ui-expressions-remove "Remove expr")
    ("se" dap-ui-expressions "List expr")
    ("so" dap-go-to-output-buffer "Output")
    ("R" dap-debug-restart "Restart")
    ("C-c C-z" dap-ui-repl "Repl")
    ("M-s q" nil "quit" :exit t))

  (set-keymap-parent ymacs-lsp-dap-running-session-mode-map dap-hydra/keymap))

(after! dap-mode
  (require 'dap-python)
  (require 'dap-lldb)

  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions controls)))

(after! dap-python
  (setq dap-python-executable "python3"))

(after! dap-ui
  (define-key! :map dap-ui-repl-mode-map
    ("M-s d" . dap-hydra))

  (after! winum
    (dolist (buffer
             (list
              dap-ui--locals-buffer
              dap-ui--sessions-buffer
              dap-ui--debug-window-buffer
              dap-ui--expressions-buffer
              dap-ui--breakpoints-buffer))
      (add-to-list 'winum-ignored-buffers buffer))))

(after! dap-lldb
  (setq dap-lldb-debug-program '("lldb-vscode-10"))

  (setq dap-lldb-debugged-program-function
        (lambda ()
          (read-file-name "Directory: " (ymacs-cpp//build-dir) nil :must-match))))
