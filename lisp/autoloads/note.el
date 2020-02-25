;; -*- lexical-binding:t -*-

;;;###autoload
(defun org/project-open ()
  (interactive)
  (let* ((src-dir (file-truename org-project-src-dir))
         (prefix-length (length (file-name-as-directory src-dir)))
         (name (ivy-read "Open note: "
                         (--map
                          (substring it prefix-length)
                          (directory-files-recursively src-dir "\\.org\\'"))
                         :history 'org-project-note-history
                         :caller 'org/project-open)))
    (setq name (abbreviate-file-name (expand-file-name name org-project-src-dir)))
    (cond ((not (string-suffix-p ".org" name))
           (user-error "filename should endswith \".org\""))
          ((file-exists-p name)
           (message "Open existing note: %s" name)
           (find-file name))
          ((y-or-n-p (format "Create new note %s" (abbreviate-file-name name)))
           (make-directory (file-name-directory name) t)
           (find-file name))
          (t
           (user-error "Nothing to do with %s" name)))))

;;;###autoload
(defun org/project-sync ()
  (interactive)
  (unless org-project-sync-command
    (user-error "Variable `org-project-sync-command' is not set."))
  (let ((default-directory org-project-base-dir))
    (compilation-start org-project-sync-command)))
