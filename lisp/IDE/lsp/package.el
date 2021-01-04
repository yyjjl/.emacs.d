;; -*- lexical-binding:t -*-

(require-packages! lsp-mode lsp-ivy)

(defvar ymacs-lsp--enabled-clients nil)

(defvar-local ymacs-lsp-format-buffer-function #'lsp-format-buffer)
(defvar-local ymacs-lsp-organize-import-function #'lsp-organize-imports)

(defvar ymacs-lsp-process-buffer-name "*LSP process*")
