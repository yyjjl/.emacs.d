;; -*- lexical-binding:t -*-

(eval-when-compile
  (require 'lsp-mode))

(declare-function flymake-diagnostic-text 'flymake)
(declare-function flymake-diagnostic-type 'flymake)
(declare-function flymake--lookup-type-property 'flymake)
(declare-function lsp--progress-status 'lsp)

(defmacro ymacs-lsp//try-enable (-name)
  `(and (eq ymacs-lsp-project-state :enabled)
        ,(let ((symbol (intern (format "ymacs-%s-lsp" -name))))
           `(not (and (boundp ',symbol)
                      (eq ,symbol :disabled))))
        (ignore-errors
          (require 'lsp)
          ;; clear multi-folders
          (setf (lsp-session-server-id->folders (lsp-session)) (ht))
          (lsp))
        (bound-and-true-p lsp-mode)
        ;; disable semantic when using lsp
        (setq ymacs-editor--inhibit-semantic t)))

(defmacro ymacs-lsp//try-enable-simple (-name &rest -condition)
  (declare (indent 1))
  (push '(is-buffer-suitable-for-coding!) -condition)
  (if (null (cdr -condition))
      (setq -condition (car -condition))
    (setq -condition `(and . ,-condition)))
  `(with-transient-hook! (hack-local-variables-hook :local t)
     (when ,-condition
       (ymacs-lsp//try-enable ,-name))))

(defun ymacs-lsp//clear-leak-handlers (-lsp-client)
  "Clear leaking handlers in LSP-CLIENT."
  (let* ((response-handlers (lsp--client-response-handlers -lsp-client))
         (response-ids (cl-loop
                        for handler being the hash-values of response-handlers using (hash-keys response-id)
                        when (> (time-convert (time-since (nth 3 handler)) 'integer)
                                (* 2 lsp-response-timeout))
                        collect response-id)))

    (when response-ids
      (message "Deleting %d handlers in %s lsp-client..."
               (length response-ids)
               (lsp--client-server-id -lsp-client))
      (dolist (response-id response-ids)
        (remhash response-id response-handlers)))))

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

(cl-defun ymacs-lsp//register-client
    (-client &key ((:package -package)) ((:enable-fn -enable-fn)))
  (add-to-list 'lsp-client-packages -package)
  (setf (alist-get -client ymacs-lsp--enabled-clients)
        (list -package -enable-fn)))

(defun ymacs-lsp//set-simple-install-fn (-client -command &optional -update-command)
  (setf
   (lsp--client-download-server-fn (ht-get lsp-clients -client))
   (lambda (_client -callback -error-callback -update?)
     (apply #'lsp-async-start-process
            -callback
            -error-callback
            (if -update?
                (or -update-command -command)
              -command)))))

(cl-defun lsp-download-install@pretty
    (-callback -error-callback &key url store-path decompress &allow-other-keys)
  (let* ((url (lsp-resolve-value url))
         (store-path (lsp-resolve-value store-path))
         (type (pcase decompress
                 (:tgz "tgz")
                 (:7z "7z")
                 (:py "py")
                 (:zip "zip")
                 (_ "wget"))))
    (lsp--info "Starting to download %s to %s..." url store-path)
    (lsp-async-start-process
     -callback -error-callback
     "bash" (expand-etc! "scripts/install_from_url") type url store-path)))
