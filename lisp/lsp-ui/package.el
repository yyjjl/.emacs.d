;; -*- lexical-binding:t -*-

(require-packages! lsp-ui dap-mode)

(defvar ymacs-lsp-ui-doc-search-distance 15)

(defvar-local ymacs-lsp--buffer-read-only nil)
(defvar-local ymacs-lsp--dap-cookie nil)
(defvar ymacs-lsp-dap-running-session-mode-map (make-sparse-keymap))

(define-minor-mode ymacs-lsp/dap-running-session-mode
  "A mode for adding keybindings to running sessions"
  nil
  nil
  ymacs-lsp-dap-running-session-mode-map
  (if ymacs-lsp/dap-running-session-mode
      (progn
        (setq ymacs-lsp--buffer-read-only buffer-read-only)
        (unless ymacs-lsp--dap-cookie
          (setq ymacs-lsp--dap-cookie
                (face-remap-add-relative 'header-line 'doom-modeline-debug-visual)))
        (setq buffer-read-only t))

    (when buffer-read-only
      (setq buffer-read-only ymacs-lsp--buffer-read-only))
    (setq ymacs-lsp--buffer-read-only nil)

    (when ymacs-lsp--dap-cookie
      (face-remap-remove-relative ymacs-lsp--dap-cookie)))

  (force-mode-line-update))
