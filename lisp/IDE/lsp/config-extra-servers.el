;;; -*- lexical-binding: t; -*-

(defun ymacs-python//pylance-environments ()
  '(("ELECTRON_RUN_AS_NODE" . "1")
    ("VSCODE_NLS_CONFIG" . "en")))

(after! (lsp-mode lsp-pyright)
  (let ((client (copy-sequence (ht-get lsp-clients 'pyright))))
    (setf (lsp--client-new-connection client)
          (lsp-stdio-connection
           (lambda ()
             (cl-list* "node"
                       (file-local-name (expand-cache! "lsp/pylance/current/extension/dist/server.bundle.hijack.js"))
                       lsp-pyright-langserver-command-args))))
    (setf (lsp--client-environment-fn client) #'ymacs-python//pylance-environments)
    (setf (lsp--client-server-id client) 'pylance)

    (ymacs-lsp//use-common-download-script client)
    (lsp-register-client client))

  (let ((client (copy-sequence (ht-get lsp-clients 'pyright-remote))))
    (setf (lsp--client-new-connection client)
          (ymacs-lsp//tramp-connection
           (lambda ()
             (cl-list* "node"
                       (file-local-name (ymacs-lsp//remote-server-command-path-nonlocal "pylance/current/extension/dist/server.bundle.hijack.js"))
                       lsp-pyright-langserver-command-args))))
    (setf (lsp--client-server-id client) 'pylance-remote)
    (lsp-register-client client))

  (when ymacs-lsp-enable-remote-server-p
    (add-to-list 'lsp-disabled-clients 'pyright-remote)))
