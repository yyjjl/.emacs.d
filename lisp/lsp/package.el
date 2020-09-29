;; -*- lexical-binding:t -*-

(require-packages! lsp-mode lsp-ivy lsp-ui dap-mode)

(define-option! ymacs-lsp-modern-ui nil)

(defvar ymacs-lsp-ui-doc-search-distance 15)

(defcustom ymacs-lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)
