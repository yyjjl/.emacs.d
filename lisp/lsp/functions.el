;; -*- lexical-binding:t -*-

(cl-defun ymacs-lsp//register-client
    (-client &key ((:manual -manual)) ((:package -package)))
  (setf (alist-get -client ymacs-lsp--enabled-clients)
        (cons -package -manual)))

(cl-defmacro ymacs-lsp//try-enable
    (name &key (enable t) (init nil) (fallback nil))
  (declare (indent 1))
  `(add-transient-hook!
       (hack-local-variables-hook
        :local t
        :name ,(intern (format "ymacs-lsp//%s-internal" name)))
     (if (and ,enable
              ymacs-lsp-enable-in-project-p
              (ignore-errors (lsp))
              (bound-and-true-p lsp-mode))
         ,init
       ,fallback)))

(defun ymacs-lsp//set-simple-install-fn (-client -command &optional -update-command)
  (unless -update-command
    (setq -update-command -command))
  (setf
   (lsp--client-download-server-fn (ht-get lsp-clients -client))
   (lambda (_client -callback -error-callback -update?)
     (condition-case err
         (run-command!
          :buffer-name ymacs-lsp-process-buffer-name
          :command (if -update? -update-command -command)
          :callback (lambda (&rest _) (funcall -callback))
          :error-callback (lambda (&rest _) (funcall -error-callback "failed")))
       (error (funcall -error-callback (error-message-string err)))))))

(cl-defun lsp-download-install@pretty
    (-callback -error-callback &key url store-path &allow-other-keys)
  (let ((url (lsp-resolve-value url))
        (store-path (lsp-resolve-value store-path)))
    (run-command!
     :buffer-name ymacs-lsp-process-buffer-name
     :command (format "proxychains wget %s -O %s" url store-path)
     :callback
     (lambda (&rest _)
       (condition-case err
           (funcall -callback)
         (error
          (funcall -error-callback (error-message-string err)))))
     :error-callback
     (lambda (_ msg) (funcall -error-callback msg)))))

(defun lsp-async-start-process@pretty (-callback -error-callback &rest -command)
  "Start async process COMMAND with CALLBACK and ERROR-CALLBACK."
  (run-command!
   :buffer-name ymacs-lsp-process-buffer-name
   :command (string-join -command " ")
   :callback
   (lambda (&rest _)
     (condition-case err
         (funcall -callback)
       (error
        (funcall -error-callback (error-message-string err)))))
   :error-callback
   (lambda (_ msg) (funcall -error-callback msg))))
