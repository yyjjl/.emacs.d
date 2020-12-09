;; -*- lexical-binding:t -*-

(after-feature! lsp
  (after! lsp-mode
    (setq lsp-headerline-breadcrumb-enable t)))

(after! lsp-mode
  (ymacs-hydra-add-toggle-column
   '(lsp-mode
     "LSP"
     (("t i" lsp-toggle-trace-io
       "trace io" :toggle lsp-print-io)
      ("t h" lsp-toggle-symbol-highlight
       "highlight" :toggle lsp-enable-symbol-highlighting)
      ("t t" lsp-toggle-on-type-formatting
       "on type formating" :toggle lsp-enable-on-type-formatting)
      ("t s" lsp-toggle-signature-auto-activate
       "signature" :toggle lsp-signature-auto-activate)

      ("t m" ymacs-lsp-ui/toggle
       "modern UI" :toggle (bound-and-true-p lsp-ui-doc-mode))

      ("w r" lsp-restart-workspace)
      ("w s" lsp-shutdown-workspace)))))

(after! dap-hydra
  (defhydra++ dap-hydra ()
    ("q" :delete)
    ("ed" dap-ui-expressions-remove "Remove expr")
    ("el" dap-ui-expressions "List expr")
    ("so" dap-go-to-output-buffer "Output")
    ("sL" ymacs-dap/go-to-log-buffer "Log")
    ("C-c C-z" dap-ui-repl "Repl")
    ("M-s q" nil "quit" :exit t)))

(after! lsp-headerline
  (setq lsp-headerline-breadcrumb-segments '(project symbols)))

(after! lsp-ui
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-imenu-enabel nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)

  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol nil)

  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-header nil)
  (setq lsp-ui-doc-include-signature nil)
  (setq lsp-ui-doc-max-width 80))

(after! dap-mode
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
