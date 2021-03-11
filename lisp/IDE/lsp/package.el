;; -*- lexical-binding:t -*-

(option! lsp-use-dap nil
  "Where to use dap-mode"
  :type 'boolean)

(require-packages!
 lsp-mode
 lsp-ivy
 (dap-mode :when ymacs-lsp-use-dap-p))

(defvar ymacs-lsp--enabled-clients nil)

(defvar-local ymacs-lsp-format-buffer-function #'lsp-format-buffer)
(defvar-local ymacs-lsp-organize-import-function #'lsp-organize-imports)
(defvar-local ymacs-lsp-find-other-file-function nil)

(defvar ymacs-lsp-process-buffer-name "*lsp process*")
(defvar ymacs-lsp-doc-buffer "*lsp-signature*")

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
