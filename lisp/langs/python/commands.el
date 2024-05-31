;;; -*- lexical-binding: t

(eval-when-has-feature! lsp
  (defun ymacs-python/change-lsp-server ()
    (interactive)
    (let* ((servers (cl-remove ymacs-python-lsp-server ymacs-python-lsp-servers))
           (server (intern (completing-read! "Server: " (mapcar #'symbol-name servers)))))
      (ymacs-python//set-lsp-server server))

    (let (buffers)
      (when-let ((workspaces (lsp-workspaces)))
        (with-lsp-workspace (lsp--read-workspace)
          (setq buffers (lsp--workspace-buffers lsp--cur-workspace))
          (lsp--shutdown-workspace)))

      (dolist (buffer buffers)
        (with-current-buffer buffer
          (revert-buffer))))))

;;;###autoload
(defun ymacs-python/setup-project ()
  (interactive)
  (let* ((project-root (read-directory-name "Root: " (ymacs-editor//project-root)))
         (venv-root (read-directory-name "VenvRoot: " python-shell-virtualenv-root))
         (python-path (read-string "Python: " python-shell-interpreter)))
    (ymacs-editor//setup-project-internal
     project-root
     (append
      (list (cons 'python-shell-interpreter python-path)
            (cons 'python-shell-virtualenv-root venv-root))
      (eval-when-has-feature! lsp
        (list (cons 'lsp-pyright-python-executable-cmd python-path)))))))

;;;###autoload
(defun ymacs-python/shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace, indent.
If not try to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point)))
      (indent-for-tab-command)
    (call-interactively #'company-complete)))

;;;###autoload
(defun ymacs-python/autopep8 ()
  (interactive)
  (let ((temporary-file-directory default-directory))
    (save-restriction
      (widen)
      (py-autopep8-buffer))))

;;;###autoload
(defun ymacs-python/toggle-pdbtrack ()
  (interactive)
  (require 'python)

  (if (memq 'python-pdbtrack-comint-output-filter-function
            comint-output-filter-functions)
      (progn
        (ymacs-python//disable-pdbtrack)
        (when (called-interactively-p 'interactive)
          (message "pdbtrack disabled")))
    (ymacs-python//enable-pdbtrack)
    (when (called-interactively-p 'interactive)
      (message "pdbtrack enabled"))))


;;;###autoload
(defun ymacs-python/pop-to-shell (&optional -directory)
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list
      (cond ((= arg 0) default-directory)
            ((= arg 16) (read-directory-name "Directory: "))
            (t (ymacs-python//get-execution-root))))))
  (let ((source-buffer (current-buffer))
        (buffer (process-buffer (ymacs-python//get-or-create-process -directory t))))
    (with-current-buffer buffer
      (setq ymacs-python--last-buffer source-buffer))
    (pop-to-buffer buffer)))

;;;###autoload
(defun ymacs-python/pop-to-source-buffer ()
  "Switch from inferior Python process buffer to recent Python buffer."
  (interactive)
  (unless (buffer-live-p ymacs-python--last-buffer)
    (user-error "Source buffer is killed"))
  (pop-to-buffer ymacs-python--last-buffer))

;;;###autoload
(defun ymacs-python/send-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (call-interactively #'ymacs-python/pop-to-shell)
    (sit-for 0.01)
    (with-current-buffer buffer
      (ymacs-python//send-region-or-buffer (= (prefix-numeric-value current-prefix-arg) 4)))))

;;;###autoload
(defun ymacs-python/extract-expression (-beg -end)
  (interactive "r")
  (let ((expr (buffer-substring -beg -end))
        (var (read-string "Variable: ")))
    (kill-new (format "%s = %s" var expr))
    (query-replace expr var nil
                   (save-excursion
                     (beginning-of-defun)
                     (point))
                   (save-excursion
                     (end-of-defun)
                     (point)))))

;;;###autoload
(defun ymacs-python/toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let* ((version
          (--> "python3 --version"
            (shell-command-to-string it)
            (split-string it)
            (nth 1 it)
            (split-string it "\\.")
            (mapcar #'string-to-number it)))
         (trace (cond ((and version (>= (or (nth 1 version) 0) 7))
                       "breakpoint()")
                      ((executable-find "ipdb")
                       "import ipdb; ipdb.set_trace()")
                      (t "import pdb; pdb.set_trace()"))))
    (or (save-excursion
          (when (ymacs-python//line-startswith-p trace)
            (kill-whole-line)
            t))
        (save-excursion
          (forward-line -1)
          (when (ymacs-python//line-startswith-p trace)
            (kill-whole-line)
            t))
        (save-excursion
          (forward-line 1)
          (when (ymacs-python//line-startswith-p trace)
            (kill-whole-line)
            t))
        (save-excursion
          (back-to-indentation)
          (insert trace "\n")
          (python-indent-line)))))

;;;###autoload
(defun ymacs-python/run-current-file ()
  (interactive)
  (let* ((venv-cmd (ymacs-python//get-venv-source-cmd))
         (cmd (format
               "%spython %s"
               (if venv-cmd venv-cmd "")
               (shell-quote-argument (buffer-file-name)))))
    (compile (format "bash -c %s" (shell-quote-argument cmd)) t)))
