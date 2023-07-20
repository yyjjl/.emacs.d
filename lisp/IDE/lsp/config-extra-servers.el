;;; -*- lexical-binding: t; -*-

(defconst ymacs-python-hijack-exit
  "process.exit = (function(oldExit) {
    return function(_) {
        console.warn('Hijack !!!');
        process.exit = oldExit; // restore
    }
}(process.exit));
")

(defun ymacs-python//pylance-connection-1 (-prefix)
  (let ((original-path (concat -prefix ".js"))
        (hijack-path (concat -prefix ".hijack.js")))
    (when (file-exists-p original-path)
      (unless (file-exists-p hijack-path)
        (with-temp-file hijack-path
          (insert ymacs-python-hijack-exit)
          (insert-file-contents-literally original-path)))
      (cl-list* "node"
                (file-local-name hijack-path)
                lsp-pyright-langserver-command-args))))

(defun ymacs-python//pylance-environments ()
  '(("ELECTRON_RUN_AS_NODE" . "1")
    ("VSCODE_NLS_CONFIG" . "en")))

(after! (lsp-mode lsp-pyright)
  (let ((client (copy-sequence (ht-get lsp-clients 'pyright))))
    (setf (lsp--client-new-connection client)
          (lsp-stdio-connection
           (lambda ()
             (ymacs-python//pylance-connection-1
              (expand-cache! "lsp/pylance/extension/dist/server.bundle")))))
    (setf (lsp--client-environment-fn client) #'ymacs-python//pylance-environments)
    (setf (lsp--client-server-id client) 'pylance)
    (setf (lsp--client-download-server-fn client)
          (lambda (_client callback error-callback _update?)
            (let ((default-directory (expand-etc! "setup")))
              (lsp-async-start-process callback error-callback "python3" "update_pylance.py"))))
    (lsp-register-client client))

  (let ((client (copy-sequence (ht-get lsp-clients 'pyright-remote))))
    (setf (lsp--client-new-connection client)
          (ymacs-lsp//tramp-connection
           (lambda ()
             (ymacs-python//pylance-connection-1
              (ymacs-lsp//remote-server-command-path-nonlocal "pylance/extension/dist/server.bundle")))))
    (setf (lsp--client-server-id client) 'pylance-remote)
    (lsp-register-client client))

  (when ymacs-lsp-enable-remote-server-p
    (add-to-list 'lsp-disabled-clients 'pyright-remote)))
