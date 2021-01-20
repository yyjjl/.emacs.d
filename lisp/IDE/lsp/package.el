;; -*- lexical-binding:t -*-

(defcustom ymacs-lsp-use-modern-ui nil
  "Where to use lsp-ui"
  :group 'ymacs
  :type 'boolean)

(defcustom ymacs-lsp-use-dap nil
  "Where to use dap-mode"
  :group 'ymacs
  :type 'boolean)

(require-packages!
 lsp-mode
 lsp-ivy
 (lsp-ui :when ymacs-lsp-use-modern-ui)
 (dap-mode :when ymacs-lsp-use-dap))

(defvar ymacs-lsp--enabled-clients nil)

(defvar-local ymacs-lsp-format-buffer-function #'lsp-format-buffer)
(defvar-local ymacs-lsp-organize-import-function #'lsp-organize-imports)

(defvar ymacs-lsp-process-buffer-name "*LSP process*")

(eval-when! ymacs-lsp-use-modern-ui
  (autoload 'ymacs-lsp/toggle-modern-ui (expand! "commands") nil t))

(eval-when! ymacs-lsp-use-dap
  (defvar ymacs-dap-running-session-mode-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map (kbd "C-c C-z") #'ymacs-dap/goto-repl-buffer)
      (define-key map (kbd "C-c z") #'ymacs-dap/goto-repl-buffer)
      map))

  (autoload 'ymacs-dap-running-session-mode (expand! "commands") nil t)
  (autoload 'ymacs-dap/goto-log-buffer (expand! "commands") nil t)
  (autoload 'ymacs-dap/goto-repl-buffer (expand! "commands") nil t))
