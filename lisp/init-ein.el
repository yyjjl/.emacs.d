(with-eval-after-load 'ein
  (defun setup-ein-function ()
    (add-to-list 'company-backends 'ein:company-backend))
  (add-hook 'ein:notebook-multilang-mode-hook
            #'setup-ein-function))

(with-eval-after-load 'ein-company
  (defun ein:company-backend (command &optional arg &rest ignore)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'ein:company-backend))
      (prefix (and (--filter (and (boundp it) (symbol-value it)
                                  (eql it 'ein:notebook-minor-mode))
                             minor-mode-list)
                   (when (looking-back "\\([*.a-zA-Z0-9_]\\)")
                     (ein:object-at-point))))
      (doc-buffer (lexical-let ((arg arg))
                    (cons :async
                          (lambda (cb)
                            (ein:company-handle-doc-buffer arg cb)))))
      (location (lexical-let ((obj arg))
                  (cons :async
                        (lambda (cb)
                          (ein:pytools-find-source (ein:get-kernel-or-error)
                                                   obj
                                                   cb)))))
      (candidates
       () (lexical-let ((kernel (ein:get-kernel-or-error))
                        (col (current-column)))
            (cons :async
                  (lambda (cb)
                    (ein:kernel-complete
                     kernel
                     (thing-at-point 'line)
                     col
                     (list :complete_reply
                           (cons #'ein:completer-finish-completing-company
                                 (list :callback cb)))))))))))

(defvar ein-default-directory "~/documents/ipynb/")
(defvar ein-process nil)
(defvar ein-executable-name "jupyter")
(defvar ein-command-args "notebook --no-browser")
(defvar ein-buffer-name "*ein*")

(defun ein-sentinel (process _event)
  "Watch the activity of EIN process."
  (let ((status (process-status process)))
    (when (memq status '(exit signal closed failed))
      (message "ein process (jupyter notebook) stopped..."))))

(defun ein-start-jupyter-unless-running ()
  ;; Already started, nothing need to be done
  (cond
   ( (or (and (processp ein-process)
             (not (eq (process-status ein-process) 'exit))
             (not (eq (process-status ein-process) 'signal)))
         (dolist (pid (reverse (list-system-processes)))
           ;; Check in the sys-processes for rdm
           (let* ((attrs (process-attributes pid))
                  (pname (cdr (assoc 'comm attrs)))
                  (uid (cdr (assoc 'euid attrs))))
             (when (and (eq uid (user-uid))
                       (string-prefix-p "jupyter-note" pname))
               (return t))))))
   ((null ein-executable-name)
    ;; Executable not found or invalid
    (message "Please set `jupyter' executable name"))
   (t
    (let ((default-directory ein-default-directory))
      (setq ein-process (start-file-process-shell-command
                         "EIN" ein-buffer-name
                         (concat ein-executable-name " " ein-command-args))))
    (set-process-query-on-exit-flag ein-process nil)
    (set-process-sentinel ein-process 'ein-sentinel))))

(defun ein-notebooklist-default ()
  (interactive)
  (if (not (ein-start-jupyter-unless-running))
      (message "Can't not find or start jupyter process"))
  (unless (ignore-errors (ein:notebooklist-open))
    (ein:notebooklist-login)
    (ein:notebooklist-open)))

(provide 'init-ein)