;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor//insert-git-files ()
  (interactive)

  (let ((default-directory (or (ymacs-editor//project-root) default-directory)))
    (unless (file-directory-p ".git")
      (user-error "No git repo found"))

    (dolist (file (thread-first "git ls-files  --full-name --"
                    (shell-command-to-string)
                    (split-string "\n" :omit-nulls "\n\t ")))
      (insert file "\n"))))

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
