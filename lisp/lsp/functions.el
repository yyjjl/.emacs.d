;; -*- lexical-binding:t -*-

(cl-defmacro ymacs-lsp//try-enable (name &key (enable t) (init nil) (fallback nil))
  (declare (indent 1))
  `(add-transient-hook! (hack-local-variables-hook :local t :name ,(intern (format "ymacs-lsp//%s-internal" name)))
     (if (and ,enable
              lsp-enable-in-project-p
              (ignore-errors (lsp))
              (bound-and-true-p lsp-mode))
         ,init
       ,fallback)))

(defun ymacs-lsp//set-simple-install-fn (client command)
  (setf (lsp--client-download-server-fn (ht-get lsp-clients client))
        (lambda (_client callback error-callback _update?)
          (condition-case err
              (run-command!
               :name (format "Install %s" client)
               :command command
               :callback (lambda (&rest _) (funcall callback))
               :error-callback (lambda (&rest _) (funcall error-callback "failed")))
            (error (funcall error-callback (error-message-string err)))))))
