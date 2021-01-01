;;; -*- lexical-binding: t; -*-

(defun ymacs-lsp//display-info-from-github (-repo)
  (require 'url-handlers)

  (let* ((url (format "https://api.github.com/repos/%s/releases/latest" -repo))
         (info (with-temp-buffer
                 (erase-buffer)
                 (message "GET %s ..." url)
                 (url-insert-file-contents url)
                 (message "GET %s ...done" url)
                 (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist))))
    (insert (format "*url* :: [[%s]]\n" (alist-get 'html_url info)))
    (insert (format "*name* :: %s\n" (alist-get 'name info)))
    (insert (format "*tag_name* :: %s\n" (alist-get 'tag_name info)))
    (insert (format "*published_at* :: %s\n" (alist-get 'published_at info)))
    (cl-loop
     for asset across (or (alist-get 'assets info) [])
     do (insert (format "- [[%s][%s]] %s\n"
                        (alist-get 'browser_download_url asset)
                        (alist-get 'name asset)
                        (if-let (size (alist-get 'size asset))
                            (file-size-human-readable size)
                          "??"))))))

(cl-defun ymacs-lsp//check-software-from-github
    (&key
       ((:title -title))
       ((:repo -repo))
       ((:exe -executable))
       ((:version -version-flag) "--version"))
  (insert (format "* %s\n" -title))
  (insert (format "*Path* :: %s\n" -executable))
  (if (file-executable-p -executable)
      (progn
        (insert (format "*Installed* :: %s\n"
                        (shell-command-to-string (concat -executable " " -version-flag))))
        (ymacs-lsp//display-info-from-github -repo))
    (insert "*Not installed*\n"))
  (insert "\n\n")
  (redisplay t))

(defun ymacs-lsp//install-clients (-clients -callback)
  (let (output-strings
        (output-buffer (get-buffer-create ymacs-lsp-process-buffer-name)))
    (cl-labels
        ((callback-fn
          (&optional _message)
          (with-current-buffer output-buffer
            (push (buffer-substring (point-min) (point-max)) output-strings))
          (funcall #'install-fn))
         (install-fn
          ()
          (let ((client (pop -clients)))
            (if client
                (progn
                  (push (format "* Install %s" client) output-strings)
                  (funcall
                   (lsp--client-download-server-fn (ht-get lsp-clients client))
                   client
                   #'callback-fn
                   #'callback-fn
                   t))
              (with-current-buffer output-buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (string-join (reverse output-strings) "\n\n") "\n\n")
                  (funcall -callback)
                  (set-buffer-modified-p nil)))))))
      (funcall #'install-fn))))

;;;###autoload
(defun ymacs-lsp/check-for-updates ()
  (interactive)

  (let (clients manual-clients)
    (cl-loop
     for (client package . manual) in ymacs-lsp--enabled-clients
     do (if manual
            (push manual manual-clients)
          (progn
            (when package
              (require package nil t))
            (push client clients))))

    (ymacs-lsp//install-clients
     clients
     (lambda ()
       (goto-char (point-max))

       (dolist (client manual-clients)
         (apply #'ymacs-lsp//check-software-from-github client))

       (org-mode)
       (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun ymacs-lsp/toggle-semantic-tokens ()
  (interactive)
  (setq lsp-semantic-tokens-enable (not lsp-semantic-tokens-enable))
  (if lsp-semantic-tokens-enable
      (lsp-semantic-tokens--enable)
    (lsp-semantic-tokens--disable)
    (font-lock-flush))
  (lsp--info "Semantic Tokens %s. "
             (if lsp-semantic-tokens-enable "enabled" "disabled")))
