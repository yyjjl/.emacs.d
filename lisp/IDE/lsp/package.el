;; -*- lexical-binding:t -*-

(option! lsp-project-state :enabled
  "Whether to enable lsp in current project"
  :type '(choice
          (const :tag "Enable LSP in current file" :enabled)
          (const :tag "Disable LSP in current file" :disabled))
  :safe #'(lambda (x) (memq x '(:enabled :disabled))))

(require-packages!
 lsp-mode
 consult-lsp)

(setq lsp-client-packages nil)

(defvar ymacs-lsp--enabled-clients nil)

(defvar-local ymacs-lsp-format-buffer-function #'lsp-format-buffer)
(defvar-local ymacs-lsp-organize-import-function #'lsp-organize-imports)
(defvar-local ymacs-lsp-find-other-file-function nil)

(defvar ymacs-lsp-install-buffer "*lsp-install*")
