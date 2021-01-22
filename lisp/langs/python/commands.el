;;; -*- lexical-binding: t

(declare-function pyvenv-workon-home 'pyvenv)

(eval-when-has-feature! lsp
  (defun ymacs-python/change-lsp-server ()
    (interactive)
    (let* ((servers (cl-remove ymacs-python-lsp-server ymacs-python-lsp-servers))
           (server (intern (completing-read! "Server: " (mapcar #'symbol-name servers)))))
      (ymacs-python//set-lsp-server server))

    (let (buffers)
      (when-let ((workspaces (lsp-workspaces)))
        (with-lsp-workspace
            (lsp--completing-read "Select server: " workspaces 'lsp--workspace-print nil t)
          (setq buffers (lsp--workspace-buffers lsp--cur-workspace))
          (lsp--shutdown-workspace)))

      (dolist (buffer buffers)
        (with-current-buffer buffer
          (revert-buffer))))))

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
(defun ymacs-python/create-venv-in-workon-home ()
  (interactive)
  (let ((directory (pyvenv-workon-home)))
    (ymacs-python/create-venv
     (expand-file-name (read-from-minibuffer (format "Directory: %s/" directory)) directory)
     (read-shell-command "Python executable: " "python")
     (when current-prefix-arg
       (read-from-minibuffer "Arguments: ")))))

;;;###autoload
(defun ymacs-python/create-venv (-directory -python-exe &optional -args)
  (interactive
   (list
    (read-directory-name "Create venv in: "
                         (ignore-errors (projectile-project-root))
                         nil :mustmatch)
    (read-shell-command "Python executable: " "python")
    (when current-prefix-arg
      (read-from-minibuffer "Arguments: "))))

  (unless (require 'pyvenv)
    (user-error "Can not load pyvenv"))

  (unless (executable-find "virtualenv")
    (user-error "Can not find virtualenv"))

  (let ((default-directory -directory))
    (compile (format "virtualenv --python=%s %s %s\n"
                     -python-exe
                     (expand-file-name "venv")
                     (or -args "")))))

;;;###autoload
(defun ymacs-python/pop-to-shell (&optional -directory)
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list
      (cond ((= arg 0) default-directory)
            ((= arg 16) (read-directory-name "Directory: "))
            (t (or (projectile-project-root) default-directory))))))
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
