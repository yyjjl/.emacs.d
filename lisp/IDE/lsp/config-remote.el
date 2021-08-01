;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'tramp)
  (require 'lsp-mode))

(option! lsp-remote-server-install-dir "~/program/lsp"
  "remote lsp server install directory"
  :type 'string)

(option! lsp-enable-remote-server nil
  "enable remote lsp server"
  :type 'boolean)

(defsubst ymacs-lsp//remote-server-command-path (-relative-path)
  (file-local-name
   (f-join (tramp-make-tramp-file-name
            (tramp-dissect-file-name default-directory)
            ymacs-lsp-remote-server-install-dir)
           -relative-path)))

(defun ymacs-lsp//tramp-connection (-local-command)
  (let ((ret (lsp-tramp-connection
              -local-command
              (lambda (name)
                (format "~/tmp/%s-%s-stderr" name (cl-incf lsp--stderr-index))))))
    (list
     :connect (plist-get ret :connect)
     :test?
     (lambda ()
       (thread-last -local-command
         lsp-resolve-final-function
         cl-first
         (tramp-make-tramp-file-name (tramp-dissect-file-name default-directory))
         file-executable-p)))))

(cl-defun ymacs-lsp//remote-npm-dependency-install (callback error-callback &key package &allow-other-keys)
  (if-let ((npm-binary (executable-find "npm" :remote)))
      (lsp-async-start-process callback
                               error-callback
                               npm-binary
                               "-g"
                               "--prefix"
                               (ymacs-lsp//remote-server-command-path (format "npm/%s" package))
                               "install"
                               package)
    (lsp-log "Unable to install %s via `npm' because it is not present" package)
    nil))

(when ymacs-lsp-enable-remote-server-p
  (after! lsp-mode
    (after! lsp-pyright
      (lsp-register-client
       (let ((client (copy-sequence (gethash 'pyright lsp-clients))))
         (setf (lsp--client-server-id client) 'pyright-remote)
         (setf (lsp--client-remote? client) t)
         (setf (lsp--client-new-connection client)
               (ymacs-lsp//tramp-connection
                (lambda ()
                  (cons
                   (ymacs-lsp//remote-server-command-path "npm/pyright/bin/pyright-langserver")
                   lsp-pyright-langserver-command-args))))
         (setf (lsp--client-download-server-fn client)
               (lambda (_client callback error-callback _update?)
                 (ymacs-lsp//remote-npm-dependency-install callback error-callback :package "pyright")))
         client)))

    (after! lsp-clangd
      (lsp-register-client
       (let ((client (copy-sequence (gethash 'clangd lsp-clients))))
         (setf (lsp--client-server-id client) 'clangd-remote)
         (setf (lsp--client-remote? client) t)
         (setf (lsp--client-new-connection client)
               (ymacs-lsp//tramp-connection
                (lambda ()
                  (cons
                   (ymacs-lsp//remote-server-command-path (format "clangd/clangd_%s/bin/clangd" lsp-clangd-version))
                   lsp-clients-clangd-args))))
         client)))

    (after! lsp-bash
      (lsp-register-client
       (let ((client (copy-sequence (gethash 'bash-ls lsp-clients))))
         (setf (lsp--client-server-id client) 'bash-ls-remote)
         (setf (lsp--client-remote? client) t)
         (setf (lsp--client-new-connection client)
               (ymacs-lsp//tramp-connection
                (lambda ()
                  (list (ymacs-lsp//remote-server-command-path "npm/bash-language-server/bin/bash-language-server")
                        "start"))))
         (setf (lsp--client-download-server-fn client)
               (lambda (_client callback error-callback _update?)
                 (ymacs-lsp//remote-npm-dependency-install callback error-callback :package "bash-language-server")))
         client)))))
