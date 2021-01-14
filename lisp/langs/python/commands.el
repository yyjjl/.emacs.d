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
  (require 'pyvenv)
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
  (require 'pyvenv)
  (when (and -dir -python-exe)
    (let* ((dir (expand-file-name ".venv" -dir))
           (command (format "virtualenv --python=%s %s %s\n"
                            -python-exe dir (or -args ""))))
      (compile command))))

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
(defun ymacs-python/kill (-arg)
  (interactive "*P")

  (with-temp-advice!
      paredit-string-start+end-points
      :around ymacs-python//around-string-start+end-points
    (paredit-kill -arg)))

;;;###autoload
(defun ymacs-python/forward-kill-word ()
  (interactive "*")

  (with-temp-advice!
      kill-word
      :before
      (lambda (&rest _)
        (let ((pos (point)))
          (when (ymacs-python//triple-quotes-p (- pos 1) (+ pos 2))
            (goto-char (+ pos 2)))))
    (paredit-forward-kill-word)))

;;;###autoload
(defun ymacs-python/backward-kill-word ()
  (interactive "*")

  (with-temp-advice!
      backward-kill-word
      :before
      (lambda (&rest _)
        (let ((pos (point)))
          (when (ymacs-python//triple-quotes-p (- pos 2) (+ pos 1))
            (goto-char (- pos 2)))))
    (paredit-backward-kill-word)))

;;;###autoload
(defun ymacs-python/backward-delete (-arg)
  (interactive "*p")
  (unless (python-indent-dedent-line)
    (let ((prefix (ymacs-python//triple-quotes-p (- (point) 3) (point)))
          (suffix (ymacs-python//triple-quotes-p (point) (+ (point) 3))))
      (cond (prefix
             (backward-char 3)
             (when (equal prefix suffix)
               (delete-char 6)))
            (t (paredit-backward-delete -arg))))))

;;;###autoload
(defun ymacs-python/forward-delete (-arg)
  (interactive "*p")
  (let ((prefix (ymacs-python//triple-quotes-p (- (point) 3) (point)))
        (suffix (ymacs-python//triple-quotes-p (point) (+ (point) 3))))
    (cond (suffix
           (forward-char 3)
           (when (equal prefix suffix)
             (delete-char -6)))
          (t (paredit-forward-delete -arg)))))

(put 'ymacs-python/backspace 'delete-selection 'supersede)

(defsubst ymacs-python//line-startswith-p (-string)
  (back-to-indentation)
  (string-prefix-p -string (buffer-substring-no-properties
                            (point)
                            (line-end-position))))

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
