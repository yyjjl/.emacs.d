;; -*- lexical-binding:t -*-

(declare-function dap-hydra 'dap-mode)

(setq lsp-keymap-prefix "C-c ;")
(after! lsp-mode
  ;; (lsp-dired-mode 1)

  (when (require 'lsp-treemacs nil t)
    (lsp-treemacs-sync-mode 1))

  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (add-to-list 'lsp-file-watch-ignored
               (eval-when-compile
                 (rx-to-string `(: string-start ,(expand-file-name "~/.local/lib") string-end))))
  (add-to-list 'lsp-file-watch-ignored
               (eval-when-compile
                 (rx-to-string `(: string-start ,(file-truename user-emacs-directory) string-end))))

  (add-to-list
   'mode-line-misc-info
   '(lsp-signature-mode
     (:propertize "[Signature]" face ymacs-modeline-lsp-success)))

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
    (";" . lsp-avy-lens)
    ("i" . ymacs-lsp/remove-invalid-folders)
    ("R" . lsp-workspace-folders-remove)
    ("b" . lsp-workspace-blacklist-remove)
    ("o" . lsp-workspace-folders-open)
    ("h" . lsp-document-highlight)
    ("l" . lsp-lens-mode))

  (defun ymacs-lsp/find-other-file ()
    (interactive)
    (unless ymacs-lsp-find-other-file-function
      (user-error "ymacs-lsp-find-other-file-function is not set"))
    (call-interactively ymacs-lsp-find-other-file-function))

  (defun ymacs-lsp/format-buffer ()
    (interactive "*")
    (call-interactively ymacs-lsp-format-buffer-function))

  (defun ymacs-lsp/organize-imports ()
    (interactive "*")
    (call-interactively ymacs-lsp-organize-import-function))

  (after! project
    (when (boundp 'project-prefix-map)
      (define-key! :map project-prefix-map
        ("a" . ymacs-lsp/find-other-file))))

  (define-key! :map lsp-mode-map
    ("M-\" r" . lsp-find-references)
    ("M-\" t" . lsp-find-type-definition)
    ("M-\" i" . lsp-find-implementation)
    ("M-\" d" . lsp-find-declaration)

    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c I" . lsp-ivy-workspace-symbol)
    ("C-c G" . lsp-ivy-global-workspace-symbol)
    ("C-c S" . lsp-describe-session)
    ("C-c b" . ymacs-lsp/organize-imports)
    ("C-c C-b" . ymacs-lsp/format-buffer)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)
    ("M-o" . lsp-signature-activate))

  (define-key! :map lsp-signature-mode-map
    ("M-o" . lsp-signature-stop))

  (setq lsp-signature-doc-lines 5)
  ;; (setq lsp-keep-workspace-alive nil)
  (setq lsp-restart 'interactive)

  ;; manually configured
  (setq lsp-completion-provider :none)
  ;; boost performance
  (setq lsp-enable-file-watchers nil)

  ;; (setq lsp-eldoc-render-all nil)
  (setq lsp-display-inline-image nil)

  ;; (setq lsp-enable-imenu t)
  ;; (setq lsp-enable-links t)
  ;; (setq lsp-enable-xref t)
  ;; (setq lsp-enable-folding t)
  ;; (setq lsp-enable-snippet t)
  ;; boost performance
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-enable-text-document-color t)
  ;; range formating
  ;; (setq lsp-enable-indentation t)
  (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-symbol-highlighting-skip-current t)

  ;; (setq lsp-before-save-edits t)

  ;; (setq lsp-semantic-tokens-enable nil)
  (setq lsp-lens-enable t)

  (setq lsp-progress-prefix " LSP:")

  ;; (setq lsp-headerline-breadcrumb-enable t)
  ;; (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil))

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
        (if (display-graphic-p)
            '(sessions locals breakpoints expressions tooltip)
          '(sessions locals breakpoints expressions))))

(after! dap-python
  (setq dap-python-executable "python3"))

(after! dap-ui
  (define-key! :map dap-ui-repl-mode-map
    ([remap indent-for-tab-command] . company-complete)
    (("C-c C-z" "C-c z") . quit-window)))

(after! dap-lldb
  (setq dap-lldb-debugged-program-function
        (lambda ()
          (read-file-name "Directory: " (ymacs-term//get-directory) nil :must-match))))

(after! treemacs
  (define-key!
    ("M-1" . ymacs-lsp/select-window-1)
    ("M-2" . ymacs-lsp/select-window-2)
    ("M-`" . treemacs))

  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-missing-project-action 'remove
        treemacs-follow-after-init t
        treemacs-no-png-images t)

  (ymacs-modeline//def-modeline treemacs (window-number major-mode))

  (setq treemacs-user-mode-line-format '(:eval (ymacs-modeline//format--treemacs)))

  ;; (treemacs-follow-mode t)
  (treemacs-tag-follow-mode 1)
  (treemacs-filewatch-mode -1))

(after! lsp-treemacs
  (define-key!
    ("C-x ' '" . lsp-treemacs-errors-list)
    ("C-x ' s" . lsp-treemacs-symbols)
    ("C-x ' r" . lsp-treemacs-references)
    ("C-x ' i" . lsp-treemacs-implementations)
    ("C-x ' c" . lsp-treemacs-call-hierarchy)))
