;; -*- lexical-binding:t -*-

(declare-function dap-hydra 'dap-mode)

(setq lsp-keymap-prefix nil)
(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (add-to-list 'lsp-file-watch-ignored (concat "\\`" (expand-file-name "~/\\.local/lib")))
  (add-to-list 'lsp-file-watch-ignored (concat "\\`" (regexp-quote (file-truename user-emacs-directory))))

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

      ("w r" lsp-workspace-restart)
      ("w s" lsp-workspace-shutdown))))

  (setq lsp-command-map
        (define-key! :map (make-sparse-keymap)
          ("'" . lsp-avy-lens)
          ("R" . lsp-workspace-folders-remove)
          ("S" . lsp-describe-session)
          ("b" . lsp-workspace-blacklist-remove)
          ("d" . dap-hydra)
          ("h" . lsp-document-highlight)
          ("l" . lsp-lens-mode)
          ("o" . lsp-workspace-folders-open)
          ("r" . lsp-workspace-restart)
          ("s" . lsp-workspace-shutdown)))

  (defun ymacs-lsp/format-buffer ()
    (interactive "*")
    (call-interactively ymacs-lsp-format-buffer-function))

  (defun ymacs-lsp/organize-imports ()
    (interactive "*")
    (call-interactively ymacs-lsp-organize-import-function))

  (define-key! :map lsp-mode-map
    ("M-s l" . lsp-lens-mode)
    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c I" . lsp-ivy-workspace-symbol)
    ("C-c G" . lsp-ivy-global-workspace-symbol)
    ("C-c S" . lsp-describe-session)
    ("C-c B" . ymacs-lsp/organize-imports)
    (("C-c b" "C-c C-b") . ymacs-lsp/format-buffer)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)
    ("C-c l" :map lsp-command-map)
    ("M-o" . lsp-signature-activate))

  (define-key! :map lsp-signature-mode-map
    ("M-o" . lsp-signature-stop))

  (setq lsp-auto-configure t)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-diagnostics-provider :auto)
  ;; (setq lsp-restart 'auto-restart)
  (setq lsp-session-file (expand-cache! "lsp-sessions"))
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

  (setq lsp-signature-doc-lines 2)
  (setq lsp-auto-execute-action nil)

  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t))
