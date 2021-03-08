;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-default/invalid-project-cache ()
  (interactive)
  (let ((project (project-current)))
    (unless project
      (user-error "No project is found"))
    (setq ymacs-default-project-cache (clrhash ymacs-default-project-cache))

    (dolist (buffer (project--buffer-list project))
      (with-current-buffer buffer
        (cl-loop
         for (var . val) in ymacs-default-project-invalidate-cache-empty-vars
         when (local-variable-if-set-p var)
         do (set var val))))

    (message "Invalidated Projectile cache for %s." project))

  (recentf-cleanup))

;;;###autoload
(defun ymacs-default/edit-dir-locals (&optional -directory)
  "Edit or create a .dir-locals.el file of the project."
  (interactive
   (list (let ((root (ymacs-default//project-root)))
           (or (and current-prefix-arg
                    (read-directory-name "Select root" root))
               root))))
  (let ((default-directory -directory))
    (condition-case nil
        (with-temp-lv-message!
            ((propertize (substitute-command-keys "\\[keyboard-quit] to open file")
                         'face font-lock-builtin-face))
          (call-interactively #'add-dir-local-variable))
      (quit
       (let ((files (dir-locals--all-files -directory)))
         (cond ((null files)
                (let ((file (expand-file-name ".dir-locals.el" -directory)))
                  (when (yes-or-no-p (format "create file %s" file))
                    (find-file file))))
               ((= (length files) 1)
                (find-file (car files)))
               (t
                (find-file (completing-read "Open file: " files nil t)))))))))
