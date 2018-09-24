;;; -*- lexical-binding: t; -*-

(require-packages! eglot)

(defun project-try-projectile (dir)
  (let ((default-directory dir))
    (cons 'transient (projectile-ensure-project (projectile-project-root)))))

(defun eglot-find-custom (method &optional extra)
  "Send request named METHOD and get cross references of the symbol under point.
EXTRA is a plist of extra parameters."
  (let* ((identifier (xref-backend-identifier-at-point 'eglot))
         (xrefs (mapcar
                 (jsonrpc-lambda (&key uri range)
                   (eglot--xref-make identifier uri (plist-get range :start)))
                 (jsonrpc-request (eglot--current-server-or-lose)
                                  method
                                  (append (eglot--TextDocumentPositionParams)
                                          extra)))))
    (if xrefs
        (xref--show-xrefs xrefs nil)
      (message "Not found for: %s" identifier))))

(with-eval-after-load 'eglot
  (add-to-list 'project-find-functions #'project-try-projectile)

  (if-let ((value '(eglot--managed-mode (" [" eglot--mode-line-format "]")))
           (item (assq 'eglot--managed-mode mode-line-misc-info)))
      (setcdr item value)
    (add-to-list 'mode-line-misc-info value))

  (define-key! :map eglot-mode-map
    ("C-c C-d" . eglot-help-at-point)
    ("C-c r" . eglot-rename)
    ("C-c C-SPC" . eglot-code-actions)
    ("C-c f x" . eglot-shutdown)
    ("C-c f s" . eglot-stderr-buffer)
    ("C-c f e" . eglot-events-buffer)
    ("C-c f r" . eglot-reconnect))

  (defclass eglot-ccls (eglot-lsp-server) ()
    :documentation "Ccls 's C/C++ langserver.")

  (cl-defmethod eglot-initialization-options ((_server eglot-ccls))
    "Passes through required cquery initialization options"
    ccls-extra-init-params)

  (when (bound-and-true-p cpp-ccls-path)
    (setcdr (assoc 'c++-mode eglot-server-programs
                   (lambda (m1 m2)
                     (or (eq m1 m2) (and (listp m1) (memq m2 m1)))))
            `(eglot-ccls ,cpp-ccls-path))))

(provide 'init-eglot)
