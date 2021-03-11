;; -*- lexical-binding:t -*-

(declare-function flymake-diagnostic-text 'flymake)
(declare-function flymake-diagnostic-type 'flymake)
(declare-function flymake--lookup-type-property 'flymake)

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
                 (`nil "wget")
                 (_ "7z"))))

    (lsp--info "Starting to download %s to %s..." url store-path)
    (lsp-async-start-process
     -callback -error-callback
     "bash"
     (expand-etc! "scripts/install_from_url")
     type
     url
     store-path)))

(defsubst ymacs-lsp//set-lsp-signature-width ()
  (setq lsp-signature-posframe-params
        (plist-put lsp-signature-posframe-params
                   :width (max 60 (min (/ (frame-width) 2) (window-width))))))

(when ymacs-editor-use-childframe-p
  (defsubst ymacs-lsp//in-bounds ()
    (and lsp--hover-saved-bounds (lsp--point-in-bounds-p lsp--hover-saved-bounds)))

  (defun ymacs-lsp//doc-hidehandler (-info)
    (let ((parent-buffer (cdr (plist-get -info :posframe-parent-buffer))))
      (and (buffer-live-p parent-buffer)
           (or (not (equal parent-buffer (current-buffer)))
               (with-current-buffer parent-buffer
                 (not (ymacs-lsp//in-bounds)))))))

  (defun ymacs-lsp//eldoc-function (-report-doc &rest _)
    (if (ymacs-lsp//in-bounds)
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

  (defun ymacs-lsp//eldoc-message (-fmt &rest -args)
    (if (or lsp-signature-mode
            (not ymacs-editor-use-childframe-p))
        (apply #'eldoc-minibuffer-message -fmt -args)
      (if -fmt
          (posframe-show
           ymacs-lsp-doc-buffer
           :string (apply #'format-message -fmt -args)
           :hidehandler #'ymacs-lsp//doc-hidehandler
           :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
           :background-color (face-attribute 'tooltip :background)
           :height 20
           :width (max 60 (min (/ (frame-width) 2) (window-width)))
           :internal-border-color "dim grey"
           :internal-border-width 1)
        (posframe-hide ymacs-lsp-doc-buffer)))))
