;; -*- lexical-binding:t -*-

(cl-defun ymacs-lsp//register-client
    (-client &key ((:manual -manual)) ((:package -package)) ((:enable-fn -enable-fn)))
  (setf (alist-get -client ymacs-lsp--enabled-clients)
        (list -package -manual -enable-fn)))

(defun lsp-async-start-process@pretty (-callback -error-callback &rest -command)
  "Start async process COMMAND with CALLBACK and ERROR-CALLBACK."
  (condition-case err
      (run-compilation!
       :-buffer-name ymacs-lsp-process-buffer-name
       :-command (string-join -command " ")
       :-callback
       (lambda (&rest _)
         (condition-case err
             (funcall -callback)
           (error
            (funcall -error-callback (error-message-string err)))))
       :-error-callback
       (lambda (_ msg) (funcall -error-callback msg)))
    (error
     (funcall -error-callback (error-message-string err)))))

(defun ymacs-lsp//set-simple-install-fn (-client -command &optional -update-command)
  (unless -update-command
    (setq -update-command -command))
  (setf
   (lsp--client-download-server-fn (ht-get lsp-clients -client))
   (lambda (_client -callback -error-callback -update?)
     (lsp-async-start-process@pretty -callback -error-callback
                                     (if -update? -update-command -command)))))

(cl-defun lsp-download-install@pretty
    (-callback -error-callback &key url store-path &allow-other-keys)
  (let ((command (format "curl %s -o %s"
                         (lsp-resolve-value url)
                         (lsp-resolve-value store-path))))
    (lsp-async-start-process@pretty -callback -error-callback command)))
