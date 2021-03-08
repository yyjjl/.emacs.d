;; -*- lexical-binding:t -*-

(option! lsp-use-modern-ui nil
  "Where to use lsp-ui"
  :type 'boolean)

(option! lsp-use-dap nil
  "Where to use dap-mode"
  :type 'boolean)

(require-packages!
 lsp-mode
 lsp-ivy
 (lsp-ui :when ymacs-lsp-use-modern-ui-p)
 (dap-mode :when ymacs-lsp-use-dap-p))

(defvar ymacs-lsp--enabled-clients nil)

(defvar-local ymacs-lsp-format-buffer-function #'lsp-format-buffer)
(defvar-local ymacs-lsp-organize-import-function #'lsp-organize-imports)
(defvar-local ymacs-lsp-find-other-file-function nil)

(defvar ymacs-lsp-process-buffer-name "*LSP process*")

(eval-when! ymacs-lsp-use-modern-ui-p
  (autoload 'ymacs-lsp/toggle-modern-ui (expand! "commands") nil t))

(eval-when! ymacs-lsp-use-dap-p
  (defvar ymacs-dap-running-session-mode-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map (kbd "C-c C-z") #'ymacs-dap/goto-repl-buffer)
      (define-key map (kbd "C-c z") #'ymacs-dap/goto-repl-buffer)
      map))

  (autoload 'ymacs-dap-running-session-mode (expand! "commands") nil t)
  (autoload 'ymacs-dap/goto-log-buffer (expand! "commands") nil t)
  (autoload 'ymacs-dap/goto-repl-buffer (expand! "commands") nil t))
