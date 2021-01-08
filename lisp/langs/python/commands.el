;;; -*- lexical-binding: t

(defun ymacs-python/shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace, indent.
If not try to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point)))
      (indent-for-tab-command)
    (call-interactively #'company-capf)))

(eval-when-has-feature! lsp
  (defun ymacs-python/change-lsp-server ()
    (interactive)
    (let* ((servers (cl-remove ymacs-python-lsp-server ymacs-python-lsp-servers))
           (server (intern
                    (completing-read-simple!
                     :-prompt "Server: "
                     :-collection (mapcar #'symbol-name servers)))))
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
(defun ymacs-python/autopep8 ()
  (interactive)
  (let ((temporary-file-directory default-directory))
    (save-restriction
      (widen)
      (py-autopep8-buffer))))

;;;###autoload
(defun ymacs-python/debug-current-file (&optional new-session)
  (interactive "P")
  (unless (ymacs-debug//resuse-session)
    (let ((default-directory
            (or (and current-prefix-arg
                     (read-directory-name "Directory: " nil nil :must-match))
                default-directory))
          (gud-chdir-before-run nil))
      (unwind-protect
          (progn
            (lv-message "Current directory: %s" default-directory)
            (call-interactively #'pdb))
        (lv-delete-window)))))

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
(defun ymacs-python/create-venv-in-workon-home (-name -python-exe &optional -args)
  (interactive
   (list (read-from-minibuffer "Name: ")
         (read-shell-command "Python executable: ")
         (when current-prefix-arg
           (read-from-minibuffer "Arguments: "))))
  (when (and -name -python-exe (> (length -name) 0))
    (let* ((dir (expand-file-name -name (pyvenv-workon-home)))
           (command (format "virtualenv --python=%s %s %s\n"
                            -python-exe dir (or -args ""))))
      (compile command))))

;;;###autoload
(defun ymacs-python/create-venv (-dir -python-exe &optional -args)
  (interactive
   (list (read-directory-name "Directory: "
                              (ignore-errors (projectile-project-root))
                              nil :mustmatch)
         (read-shell-command "Python executable: ")
         (when current-prefix-arg
           (read-from-minibuffer "Arguments: "))))
  (when (and -dir -python-exe)
    (let* ((dir (expand-file-name ".venv" -dir))
           (command (format "virtualenv --python=%s %s %s\n"
                            -python-exe dir (or -args ""))))
      (compile command))))

;;;###autoload
(defun ymacs-python/generate-doc-at-point ()
  (interactive)
  (let (params indent insert-point)
    (save-excursion
      (if (re-search-backward
           "^\\( *\\)def[^(]+(\\([^\n]*\\))[^:]*: *$" nil t)
          (progn
            (setq params (match-string-no-properties 2))
            (setq indent (concat (match-string-no-properties 1)
                                 (make-string python-indent-offset
                                              (string-to-char " "))))
            (setq insert-point (point)))
        (message "Can not find `def'")))
    (when params
      (goto-char insert-point)
      (forward-line)
      (insert indent "\"\"\""
              "\n" indent
              (ymacs-python//generate-doc params indent)
              "\n" indent
              "\"\"\""))))

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
    (sit-for 0.1)
    (with-current-buffer buffer
      (ymacs-python//send-region-or-buffer (= (prefix-numeric-value current-prefix-arg) 4)))))

;;;###autoload
(defun ymacs-python/toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                     ((executable-find "python3.7") "breakpoint()")
                     ((executable-find "python3.8") "breakpoint()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (back-to-indentation)
      (insert trace)
      (insert "\n")
      (python-indent-line))))