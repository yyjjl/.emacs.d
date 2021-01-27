;; -*- lexical-binding:t -*-

(defun ymacs-lsp//get-latest-url-from-github (-repo -matcher)
  (require 'url-handlers)
  (let* ((url (format "https://api.github.com/repos/%s/releases/latest" -repo))
         (info (with-temp-buffer
                 (erase-buffer)
                 (lsp--info "GET %s ..." url)
                 (url-insert-file-contents url)
                 (lsp--info "GET %s ...done" url)
                 (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist))))
    (cl-loop
     for asset across (or (alist-get 'assets info) [])
     for url = (alist-get 'browser_download_url asset)
     when (and (stringp url)
               (funcall -matcher url))
     return url)))

(defun ymacs-lsp//make-download-from-github-fn (-repo -matcher &optional -decompress)
  (lambda (_client -callback -error-callback _update?)
    (if-let (url (ymacs-lsp//get-latest-url-from-github -repo -matcher))
        (lsp-download-install
         -callback
         -error-callback
         :url url
         :store-path lsp-server-install-dir
         :decompress -decompress)
      (funcall -error-callback (format "Can't not find download url for %s" -repo)))))

(cl-defun ymacs-lsp//register-client
    (-client &key ((:package -package)) ((:enable-fn -enable-fn)))
  (setf (alist-get -client ymacs-lsp--enabled-clients)
        (list -package -enable-fn)))

(defun lsp-async-start-process@pretty (-callback -error-callback &rest -command)
  "Start async process COMMAND with CALLBACK and ERROR-CALLBACK."
  (run-compilation!
   :-buffer-name ymacs-lsp-process-buffer-name
   :-command (string-join -command " ")
   :-callback (lambda (&rest _) (funcall -callback))
   :-error-callback (lambda (_ -msg) (funcall -error-callback -msg))))

(defun ymacs-lsp//set-simple-install-fn (-client -command &optional -update-command)
  (setf
   (lsp--client-download-server-fn (ht-get lsp-clients -client))
   (lambda (_client -callback -error-callback -update?)
     (lsp-async-start-process@pretty
      -callback
      -error-callback
      (if -update?
          (or -update-command -command)
        -command)))))

(cl-defun lsp-download-install@pretty
    (-callback -error-callback &key url store-path decompress &allow-other-keys)
  (let* ((url (lsp-resolve-value url))
         (store-path (lsp-resolve-value store-path))
         (command
          (pcase decompress
            (:tgz (format "wget -O- %s | tar zxf - -C %s" url store-path))
            (`nil (format "wget %s -O %s" url store-path))
            (_ (let ((tmp-path "/tmp/lsp$$"))
                 (format "trap \"rm %2$s\" EXIT; wget %1$s -O %2$s && 7z x %2$s -o%3$s"
                         url tmp-path store-path))))))

    (lsp--info "Starting to download %s to %s..." url store-path)
    (lsp-async-start-process@pretty -callback -error-callback command)))
