;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/goto-last-point ()
  (interactive)
  (let ((old-point (point)))
    (with-demoted-errors "%s"
      (call-interactively #'goto-last-change))
    (if (eq (point) old-point)
        (call-interactively #'pop-to-mark-command)
      (message "goto last change"))))

;;;###autoload
(defun ymacs-editor/find-file-externally (-files)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive (list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                       (list (buffer-file-name)))))
  (when (or (<= (length -files) 5)
            (y-or-n-p "Open more than 5 files? "))
    (dolist (file -files)
      (counsel-find-file-extern file))))


;;;###autoload
(defun ymacs-editor/edit-dir-locals (&optional -directory)
  "Edit or create a .dir-locals.el file of the project."
  (interactive
   (list (let ((root (projectile-project-root)))
           (or (and current-prefix-arg
                    (read-directory-name "Select root" root))
               root))))
  (let ((default-directory -directory))
    (condition-case nil
        (call-interactively #'add-dir-local-variable)
      (quit
       (let ((files (dir-locals--all-files -directory)))
         (cond ((null files)
                (let ((file (expand-file-name ".dir-locals.el" -directory)))
                  (when (yes-or-no-p (format "create file %s" file))
                    (find-file file)
                    (when (not (file-exists-p file))
                      (unwind-protect
                          (projectile-skel-dir-locals)
                        (save-buffer))))))
               ((= (length files) 1)
                (find-file (car files)))
               (t
                (find-file (completing-read "Open file: " files nil t)))))))))


;;;###autoload
(defun ymacs-editor/rsync-project (-local-path -remote-path -sync-to-remote)
  (interactive
   (list (read-directory-name
          "Local path: "
          ymacs-editor-project-rsync-local-path)
         (read-string
          "Remote path: "
          ymacs-editor-project-rsync-remote-path)
         (completing-read
          "direction:" '("local  -> remote" "remote -> local") nil t)))

  (when (not (file-directory-p -local-path))
    (user-error "'%s' doesn't exist." -local-path))

  (let* ((default-directory -local-path)
         (options (if (not current-prefix-arg)
                      (cl-list* "--dry-run" "--update" ymacs-editor-project-rsync-extra-options)
                    ymacs-editor-project-rsync-extra-options))
         (compile-command
          (format ymacs-editor-project-rsync-command
                  (string-join options " ")
                  (if -sync-to-remote -local-path -remote-path)
                  (if -sync-to-remote -remote-path -local-path))))
    (call-interactively #'compile)))
