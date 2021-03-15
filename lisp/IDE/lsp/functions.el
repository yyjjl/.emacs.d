;; -*- lexical-binding:t -*-

(eval-when-compile
  (require 'lsp-mode))

(declare-function flymake-diagnostic-text 'flymake)
(declare-function flymake-diagnostic-type 'flymake)
(declare-function flymake--lookup-type-property 'flymake)

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
        (bound-and-true-p lsp-mode)))

(defmacro ymacs-lsp//try-enable-simple (-name &rest -condition)
  (declare (indent 1))
  (push '(is-buffer-suitable-for-coding!) -condition)
  (if (null (cdr -condition))
      (setq -condition (car -condition))
    (setq -condition `(and . ,-condition)))
  `(with-transient-hook! (hack-local-variables-hook :local t)
     (when ,-condition
       (ymacs-lsp//try-enable ,-name))))

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
                 (_ "wget"))))

    (lsp--info "Starting to download %s to %s..." url store-path)
    (lsp-async-start-process
     -callback -error-callback
     "bash"
     (expand-etc! "scripts/install_from_url")
     type
     url
     store-path)))

(defun ymacs-lsp//eldoc-function (-report-doc &rest _)
  (if (and lsp--hover-saved-bounds (lsp--point-in-bounds-p lsp--hover-saved-bounds))
      (funcall -report-doc lsp--eldoc-saved-message)
    (setq lsp--hover-saved-bounds nil
          lsp--eldoc-saved-message nil)
    (when (not (looking-at "[[:space:]\n]"))
      (lsp-request-async
       "textDocument/hover"
       (lsp--text-document-position-params)
       (-lambda ((hover &as &Hover? :range? :contents))
         (when hover
           (when range?
             (setq lsp--hover-saved-bounds (lsp--range-to-region range?)))
           (funcall -report-doc
                    (setq lsp--eldoc-saved-message
                          (and contents
                               (lsp--render-on-hover-content
                                contents
                                lsp-eldoc-render-all))))))
       :error-handler #'ignore
       :mode 'tick
       :cancel-token :eldoc-hover))))
