;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/invalid-project-cache ()
  (interactive)
  (let ((project (project-current)))
    (unless project
      (user-error "No project is found"))
    (setq ymacs-editor-project-cache (clrhash ymacs-editor-project-cache))

    (dolist (buffer (project--buffer-list project))
      (with-current-buffer buffer
        (cl-loop
         for (var . val) in ymacs-editor-project-invalidate-cache-empty-vars
         when (local-variable-if-set-p var)
         do (set var val))))

    (message "Invalidated Projectile cache for %s." project))

  (recentf-cleanup))

;;;###autoload
(defun ymacs-editor/edit-dir-locals (&optional -directory)
  "Edit or create a .dir-locals.el file of the project."
  (interactive
   (list (let ((root (ymacs-editor//project-root)))
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

(defun ymacs-editor//generate-tags (-directory -tag-file)
  (let ((default-directory -directory)
        (tag-file (expand-file-name -tag-file -directory)))
    (run-process!
     :name "etags"
     :program "bash"
     :program-args (list (expand-etc! "scripts/generate_tags.sh") tag-file)
     :callback
     (lambda (&rest _)
       (message ">>> %s" tag-file))
     :error-callback
     (lambda (&rest _)
       (message "FAILED >>> %s" tag-file)))))

;;;###autoload
(defun ymacs-editor/generate-tags (-directory -tag-file)
  (interactive
   (list (or (when current-prefix-arg
               (read-directory-name "Root directory: " nil nil :mustmatch))
             (ymacs-editor//project-root)
             default-directory)
         (if current-prefix-arg
             (read-string "Tag file: " "TAGS")
           "TAGS")))
  (let ((root (ymacs-editor//project-root -directory)))
    (when (or root
              (yes-or-no-p (format "Are you sure to generate tags for %s" -directory)))
      (if (get-process "etags")
          (message "etags is running ... please retry after a while")
        (ymacs-editor//generate-tags -directory -tag-file)))))
