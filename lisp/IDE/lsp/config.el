;; -*- lexical-binding:t -*-

(declare-function dap-hydra 'dap-mode)

(setq lsp-keymap-prefix "C-c l")
(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (add-to-list 'lsp-file-watch-ignored
               (eval-when-compile
                 (rx-to-string `(: string-start ,(expand-file-name "~/.local/lib") string-end))))
  (add-to-list 'lsp-file-watch-ignored
               (eval-when-compile
                 (rx-to-string `(: string-start ,(file-truename user-emacs-directory) string-end))))

  (ymacs-editor//add-toggles
   "LSP" 'lsp-mode
   '("l i" lsp-toggle-trace-io
     "Trace IO" :toggle lsp-print-io)
   '("l h" lsp-toggle-symbol-highlight
     "Highlight Symbol" :toggle lsp-enable-symbol-highlighting)
   '("l t" lsp-toggle-on-type-formatting
     "On Type Formating" :toggle lsp-enable-on-type-formatting)
   '("l s" lsp-toggle-signature-auto-activate
     "Signature" :toggle lsp-signature-auto-activate)
   '("l S" ymacs-lsp/toggle-semantic-tokens
     "Semantic" :toggle lsp-semantic-tokens-enable)

   '("l r" lsp-workspace-restart "Restart" :exit t)
   '("l K" lsp-workspace-shutdown "Shutdown" :exit t)
   '("l U" ymacs-lsp/check-for-updates "Update All Servers" :exit t)
   '("l I" lsp-install-server "Install Server" :exit t))

  (define-key! :map lsp-command-map
    ("'" . lsp-avy-lens)
    ("R" . lsp-workspace-folders-remove)
    ("b" . lsp-workspace-blacklist-remove)
    ("o" . lsp-workspace-folders-open)
    ("h" . lsp-document-highlight)
    ("l" . lsp-lens-mode))

  (defun ymacs-lsp/format-buffer ()
    (interactive "*")
    (call-interactively ymacs-lsp-format-buffer-function))

  (defun ymacs-lsp/organize-imports ()
    (interactive "*")
    (call-interactively ymacs-lsp-organize-import-function))

  (define-key! :map lsp-mode-map
    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c I" . lsp-ivy-workspace-symbol)
    ("C-c G" . lsp-ivy-global-workspace-symbol)
    ("C-c S" . lsp-describe-session)
    ("C-c B" . ymacs-lsp/organize-imports)
    (("C-c b" "C-c C-b") . ymacs-lsp/format-buffer)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)
    ("M-o" . lsp-signature-activate))

  (define-key! :map lsp-signature-mode-map
    ("M-o" . lsp-signature-stop))

  (setq lsp-auto-configure t)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-diagnostics-provider :auto)
  (setq lsp-restart 'interactive)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-signature-doc-lines 2)
  (setq lsp-signature-auto-activate '(:on-trigger-char))
  (setq lsp-idle-delay 0.5)

  (setq lsp-completion-provider :none)

  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-folding t)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-text-document-color t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-symbol-highlighting-skip-current t)
  (setq lsp-semantic-tokens-enable nil)

  (setq lsp-auto-execute-action nil)

  (setq lsp-modeline-workspace-status-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t))

(eval-when! ymacs-lsp-use-modern-ui
  (after! lsp-ui
    (ymacs-editor//add-toggles
     "LSP" 'lsp-mode
     '("l d" ymacs-lsp/toggle-modern-ui "Modern UI" :toggle lsp-ui-mode))

    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-peek-enable nil)
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-position 'at-point)
    (setq lsp-ui-doc-alignment 'window)
    (setq lsp-ui-doc-max-height 20)
    (setq lsp-ui-doc-delay eldoc-idle-delay)))

(eval-when! ymacs-lsp-use-dap
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

    (define-key! :map lsp-command-map
      ("d" . dap-hydra))

    (setq dap-auto-configure-features
          '(sessions locals breakpoints expressions tooltip)))

  (after! dap-python
    (setq dap-python-executable "python3"))

  (after! dap-ui
    (define-key! :map dap-ui-repl-mode-map
      ([remap indent-for-tab-command] . company-complete)
      ("C-c C-z" . quit-window)))

  (after! dap-lldb
    (setq dap-lldb-debugged-program-function
          (lambda ()
            (read-file-name "Directory: " (ymacs-term//get-directory) nil :must-match)))))
