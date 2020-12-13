;; -*- lexical-binding:t -*-

(require-packages! lsp-mode lsp-ivy)

(defvar ymacs-lsp--enabled-clients nil)

(defvar ymacs-lsp-process-buffer-name "*LSP process*")
(defcustom ymacs-lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)
