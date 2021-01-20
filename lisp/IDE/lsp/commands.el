;;; -*- lexical-binding: t; -*-

(declare-function lsp-semantic-tokens--disable 'lsp-mode)

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

(defun ymacs-lsp//install-clients--loop (-clients -manual-clients &optional -outputs)
  (if-let (client (car -clients))
      (progn
        (push (format "* Install %s" client) -outputs)

        (let ((install-fn (lsp--client-download-server-fn (ht-get lsp-clients client)))
              (callback-fn
               (lambda (&rest _)
                 (ymacs-lsp//install-clients--callback -clients -manual-clients -outputs))))
          (funcall install-fn client callback-fn callback-fn t)))

    (ymacs-lsp//install-clients--finish -manual-clients -outputs)))

(defun ymacs-lsp//install-clients--callback (-clients -manual-clients -outputs)
  (ymacs-lsp//install-clients--loop
   (cdr -clients)
   -manual-clients
   (let ((buffer (get-buffer-create ymacs-lsp-process-buffer-name)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (push (buffer-substring (point-min) (point-max)) -outputs)))
     -outputs)))

(defun ymacs-lsp//install-clients--finish (-manual-clients -outputs)
  (with-current-buffer (get-buffer-create ymacs-lsp-process-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-join (reverse -outputs) "\n\n") "\n\n")
      (goto-char (point-max))

      (dolist (client -manual-clients)
        (apply #'ymacs-lsp//check-software-from-github client))

      (org-mode)
      (pop-to-buffer (current-buffer))
      (set-buffer-modified-p nil))))

;;;###autoload
(defun ymacs-lsp/check-for-updates ()
  (interactive)

  (let (clients manual-clients)
    (cl-loop
     for (client package manual enable-fn) in ymacs-lsp--enabled-clients
     when (or (null enable-fn)
              (funcall enable-fn))
     do (if manual
            (push manual manual-clients)
          (progn
            (when package
              (require package nil t))
            (push client clients))))

    (ymacs-lsp//install-clients--loop clients manual-clients)))

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

(eval-when! ymacs-lsp-use-modern-ui
  (defun ymacs-lsp/toggle-modern-ui ()
    (interactive)

    (if (bound-and-true-p lsp-ui-mode)
        (progn
          (unless eldoc-mode
            (eldoc-mode 1))
          (lsp-ui-mode -1))
      (eldoc-mode -1)
      (lsp-ui-mode 1))))


(eval-when! ymacs-lsp-use-dap
  (define-minor-mode ymacs-dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil nil
    ymacs-dap-running-session-mode-map
    (if ymacs-dap-running-session-mode
        (ymacs-debug//enable)
      (ymacs-debug//disable))

    (force-mode-line-update))

  (defun ymacs-dap/goto-log-buffer ()
    (interactive)
    (let ((session (dap--cur-session-or-die)))
      (when-let* ((proc (dap--debug-session-program-proc session))
                  (buffer (process-buffer proc)))
        (pop-to-buffer buffer))))

  (defun ymacs-dap/goto-repl-buffer ()
    (interactive)
    (dap-hydra/nil)
    (if-let ((buffer (get-buffer dap-ui--repl-buffer))
             (window (get-buffer-window buffer)))
        (if (eq window (selected-window))
            (quit-window)
          (select-window window))
      (dap-ui-repl))))
