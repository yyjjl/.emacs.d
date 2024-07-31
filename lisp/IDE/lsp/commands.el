;;; -*- lexical-binding: t; -*-

(declare-function treemacs-current-visibility 'treemacs)
(declare-function treemacs-select-window 'treemacs)

;;;###autoload
(defun ymacs-lsp/change-python-server ()
  (interactive)
  (let* ((servers (cl-remove ymacs-python-lsp-server ymacs-python-lsp-servers))
         (server (intern (completing-read! "Server: " (mapcar #'symbol-name servers)))))
    (ymacs-lsp//set-python-lsp-server server))

  (let (buffers)
    (when-let ((server (eglot-current-server)))
      (setq buffers (eglot--managed-buffers server))
      (eglot-shutdown server nil))

    (dolist (buffer buffers)
      (with-current-buffer buffer
        (revert-buffer)))))

;;;###autoload
(defun ymacs-lsp//clangd-find-other-file (&optional -new-window)
  "Switch between the corresponding C/C++ source and header file.
If NEW-WINDOW (interactively the prefix argument) is non-nil,
open in a new window.

Only works with clangd."
  (interactive "P")
  (if-let* ((server (eglot--current-server-or-lose))
            (other-file (eglot--request server :textDocument/switchSourceHeader (eglot--TextDocumentIdentifier)))
            (path (eglot-uri-to-path other-file)))
      (if (file-exists-p path)
          (funcall (if -new-window #'find-file-other-window #'find-file) path)
        (user-error "Could not find other file (%s)" path))
    (user-error "Could not find other file (server no result)")))

;;;###autoload
(defun ymacs-lsp/install-lsp-server ()
  (interactive)
  (let* ((default-directory (expand-etc! "scripts"))
         (valid-servers
          '("pylance" "pyright"
            "lsp-booster"
            "bash-language-server"
            "typescript"
            "typescript-language-server"
            "pyright-langserver"
            "cmakels" "pyls" "gopls" "hls" "texlab"))
         (server (completing-read "server: " valid-servers nil t)))
    (run-compilation!
     :buffer-name (format "*lsp install server: %s*" server)
     :command (format "bash update_or_install_lsp %s" server)
     :callback (lambda (_msg) (message "Install %s success" server)))))

;;;###autoload
(defun ymacs-lsp/select-window-1 ()
  (interactive)
  (let ((path (window-parameter (selected-window) 'ace-window-path)))
    (if (and (equal path "1")
             (eq 'visible (treemacs-current-visibility)))
        (treemacs-select-window)
      (ymacs-editor/aw-select-window))))

;;;###autoload
(defun ymacs-lsp/select-window-2 ()
  (interactive)
  (or (ymacs-editor//aw-switch-window-internal "2")
      (ymacs-editor//aw-switch-window-internal "1")
      (message "No specified window: 1 or 2")))
