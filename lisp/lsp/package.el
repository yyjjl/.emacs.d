;; -*- lexical-binding:t -*-

(require-packages! lsp-mode lsp-ivy dap-mode)

(defcustom lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)
