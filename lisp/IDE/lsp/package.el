;; -*- lexical-binding:t -*-

(option! lsp-project-state :enabled
  "Whether to enable lsp in current project"
  :type '(choice
          (const :tag "Enable LSP in current file" :enabled)
          (const :tag "Disable LSP in current file" :disabled))
  :safe #'(lambda (x) (memq x '(:enabled :disabled))))

(option! lsp-use-dap nil
  "Where to use dap-mode"
  :type 'boolean)

(require-packages!
 lsp-mode
 lsp-ivy
 dap-mode)

(setq lsp-client-packages nil)

(defvar ymacs-lsp--enabled-clients nil)

(defvar-local ymacs-lsp-format-buffer-function #'lsp-format-buffer)
(defvar-local ymacs-lsp-organize-import-function #'lsp-organize-imports)
(defvar-local ymacs-lsp-find-other-file-function nil)

(defvar ymacs-lsp-install-buffer "*lsp-install*")

(defvar ymacs-dap-running-session-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "C-c C-z") #'ymacs-dap/goto-repl-buffer)
    (define-key map (kbd "C-c z") #'ymacs-dap/goto-repl-buffer)
    map))

(autoload 'ymacs-dap-running-session-mode (expand! "commands") nil t)
(autoload 'ymacs-dap/goto-log-buffer (expand! "commands") nil t)
(autoload 'ymacs-dap/goto-repl-buffer (expand! "commands") nil t)
