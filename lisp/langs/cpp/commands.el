;;; -*- lexical-binding: t; -*-

(defvar gud-chdir-before-run)

;;;###autoload
(defun ymacs-cpp/load-in-repl ()
  (interactive)
  (when (file-remote-p default-directory)
    (user-error "Not support in remove sever !"))
  (unless (executable-find "root")
    (user-error "Executable `root' not found !"))
  (unless (buffer-file-name)
    (user-error "Buffer has no file !"))

  (ymacs-term//exec-program-reuse-buffer
   (concat "ROOT:" (buffer-name))
   "root" (list "-l" (or (buffer-file-name) ""))
   :-callback
   (lambda () (ymacs-term//send-string (format ".X %s\n" (buffer-file-name))))))

;;;###autoload
(defun ymacs-cpp/debug-current-file (&optional -new-session)
  (interactive "P")
  (unless (ymacs-debug//resuse-session)
    (let* ((build-dir (ymacs-cpp//build-dir))
           (default-directory
             (or (and -new-session
                      (--> build-dir
                           (or it default-directory)
                           (file-name-as-directory it)
                           (read-directory-name "Directory: " it nil :must-match)
                           (expand-file-name it)))
                 build-dir
                 default-directory))
           (gud-chdir-before-run nil))
      (unwind-protect
          (progn
            (lv-message "Current directory: %s" default-directory)
            (call-interactively #'gdb))
        (lv-delete-window)))))

;;;###autoload
(defun ymacs-cpp/electric-star (-arg)
  (interactive "*P")
  (if (eq (char-before) ?\/)
      (progn
        (self-insert-command (prefix-numeric-value -arg))
        (insert "  */")
        (backward-char 3)
        (indent-according-to-mode))
    (call-interactively 'self-insert-command)))

;;;###autoload
(defun ymacs-cpp/get-include-paths ()
  (interactive)
  (message "%s" (shell-command-to-string (expand-etc! "scripts/C_include_paths"))))
