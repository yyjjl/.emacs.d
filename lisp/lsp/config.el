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

      ("t m" ymacs-lsp/toggle-modern-ui
       "modern UI" :toggle (bound-and-true-p lsp-ui-doc-mode))

      ("w r" lsp-restart-workspace)
      ("w s" lsp-shutdown-workspace))))

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
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-code-actions-enable t))

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

(after! dap-hydra
  (defhydra++ dap-hydra ()
    ("q" :delete)
    ("ed" dap-ui-expressions-remove "Remove expr")
    ("el" dap-ui-expressions "List expr")
    ("so" dap-go-to-output-buffer "Output")
    ("sL" ymacs-dap/go-to-log-buffer "Log")
    ("C-c C-z" dap-ui-repl "Repl")
    ("M-s q" nil "quit" :exit t)))

(after! lsp-cmake
  (ymacs-lsp//set-simple-install-fn
   'cmakels
   "pip3 install --user cmake-language-server"))

(after! lsp-python
  (ymacs-lsp//set-simple-install-fn
   'pyls
   "pip3 install --user 'python-language-server[all]"))

(after! ccls
  (ymacs-lsp//set-simple-install-fn
   'ccls
   (expand-etc! "setup/install_ccls.sh")))
