;; -*- lexical-binding:t -*-

(after! dap-hydra
  (defhydra++ dap-hydra ()
    ("q" :delete)
    ("ed" dap-ui-expressions-remove "Remove expr")
    ("se" dap-ui-expressions "List expr")
    ("so" dap-go-to-output-buffer "Output")
    ("si" ymacs-dap/goto-log-buffer "Input")
    ("R" dap-debug-restart "Restart")
    ("M-s q" nil "quit" :exit t))

  (set-keymap-parent ymacs-dap-running-session-mode-map dap-hydra/keymap))

(after! dap-mode
  (require 'dap-python)
  (require 'dap-lldb)

  (define-key! :map lsp-mode-map
    ("M-s d" . dap-hydra))

  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions tooltip)))

(after! dap-python
  (setq dap-python-executable "python3"))

(after! dap-ui
  (define-key! :map dap-ui-repl-mode-map
    ([remap indent-for-tab-command] . company-complete)
    ("C-c C-z" . quit-window)))

(after! dap-lldb
  (setq dap-lldb-debug-program '("lldb-vscode-10"))

  (setq dap-lldb-debugged-program-function
        (lambda ()
          (read-file-name "Directory: " (ymacs-term//get-directory) nil :must-match))))
