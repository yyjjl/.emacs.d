;; -*- lexical-binding:t -*-

(eval-when-compile
  (require 'python))

(defun ymacs-lsp//default-workspace-configuration (-server)
  (let ((modes (eglot--major-modes -server)))
    (cond
     ((memq 'python-mode modes)
      (let ((exec-path (python-shell-calculate-exec-path)))
        `(
          :python  (
                    :venvPath ,python-shell-virtualenv-root
                    :pythonPath ,(executable-find python-shell-interpreter)
                    :analysis (:typeCheckingMode "basic"))
          :pyls (:configurationSources ["flake8"]))))
     ((memq 'tex-mode modes)
      (let ((root (or (when (and (boundp 'TeX-master) (stringp TeX-master))
                        (file-name-directory TeX-master))
                      ".")))
        `(:latex (:rootDirectory ,root :lint (:onSave :json-false))))))))


(cl-defun ymacs-lsp//set-python-lsp-server (&optional -server)
  (when (not -server)
    (if (get 'ymacs-python-lsp-server 'initialized)
        (cl-return-from ymacs-lsp//set-python-lsp-server)
      ;; first call to this function
      (setq -server ymacs-python-lsp-server)
      (setq ymacs-python-lsp-server nil)
      (put 'ymacs-python-lsp-server 'initialized t)))

  (cl-assert (memq -server ymacs-python-lsp-servers)
             "Not in %s"
             ymacs-python-lsp-servers)

  (when (and (not (eq -server ymacs-python-lsp-server)))
    (require 'eglot)
    (setq ymacs-python-lsp-server -server)
    (setf (cdr (ymacs-lsp//eglot-lookup-mode 'python-mode))
          (pcase -server
            ('pylance
             (list "node" (file-local-name (expand-cache! "lsp/pylance/current/extension/dist/server.bundle.hijack.js")) "--stdio"))
            ('pyright
             (list "node" (file-local-name (expand-cache! "lsp/npm/pyright/bin/pyright-langserver")) "--stdio"))
            ('pyls
             (list "pyls"))))))

(defun ymacs-lsp//eglot-lookup-mode (-mode)
  (let ((programs eglot-server-programs))
    (catch 'found
      (while (and programs)
        (let* ((item (car programs))
               (modes (car item)))
          (when (or (eq -mode modes)
                    (and (listp modes)
                         (or (memq -mode modes)
                             (memq -mode (mapcar #'car-safe modes)))))
            (throw 'found item)))
        (setq programs (cdr programs)))
      nil)))

(defmacro ymacs-lsp//try-enable-eglot (-name &rest -body)
  (declare (indent 1))
  (let ((hook-name (intern (format "ymacs-lsp//eglot-setup--%s" -name))))
    `(when (and (eq ymacs-lsp-project-state :enabled)
                ,(let ((symbol (intern (format "ymacs-%s-lsp" -name))))
                   `(not (and (boundp ',symbol)
                              (eq ,symbol :disabled)))))
       (defun ,hook-name ()
         (when eglot--managed-mode
           ,@-body
           (eglot-inlay-hints-mode -1)))
       (add-hook 'eglot-managed-mode-hook ',hook-name nil t)

       (eglot-ensure))))
