;;; -*- lexical-binding: t; -*-

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
  (require 'realgud nil :noerror)
  (let ((cmd-bufs (cl-remove-if-not (lambda (x)
                                      (and (realgud-cmdbuf? x)
                                           (process-live-p (get-buffer-process x))))
                                    (buffer-list))))
    (if (or new-session (not cmd-bufs))
        (let ((default-directory
                (or (and current-prefix-arg default-directory)
                    (expand-file-name
                     (read-directory-name "Directory: " nil nil :must-match)))))
          (realgud:pdb
           (read-shell-command "Run pdb like this: "
                               (ignore-errors (car realgud:pdb-minibuffer-history))
                               'realgud:pdb-minibuffer-history)))
      (unless realgud-short-key-mode
        (realgud-short-key-mode 1)))))

;;;###autoload
(defun ymacs-python/toggle-pdbtrack ()
  (interactive)
  (if (memq 'python-pdbtrack-comint-output-filter-function
            comint-output-filter-functions)
      (progn
        (remove-hook 'comint-output-filter-functions
                     #'python-pdbtrack-comint-output-filter-function)
        (message "pdbtrack disabled"))
    (add-hook 'comint-output-filter-functions
              #'python-pdbtrack-comint-output-filter-function)
    (message "pdbtrack enabled")))

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
            (t (or (elpy-project-root) default-directory))))))
  (let ((elpy-project-root -directory))
    (elpy-shell-switch-to-shell)))

;;;###autoload
(defun ymacs-python/send-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (call-interactively #'ymacs-python/pop-to-shell)
    (sit-for 0.1)
    (with-current-buffer buffer
      (elpy-shell-send-region-or-buffer
       (= (prefix-numeric-value current-prefix-arg) 4)))))

;;;###autoload
(defun ymacs-python/profile-buffer (&optional -directory)
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list
      (cond ((= arg 0) default-directory)
            ((>= arg 4) (read-directory-name "Directory: "))
            (t (projectile-ensure-project (projectile-project-root)))))))
  (let ((default-directory -directory)
        (python-shell-interpreter "python3"))
    (elpy-profile--file (buffer-file-name) t)))

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