;;; -*- lexical-binding: t; -*-

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

(defun ymacs-editor/generate-ctags-for-project (-tag-file)
  (interactive
   (let ((root (ymacs-editor//project-root)))
     (list (or (when (not current-prefix-arg)
                 (citre-tags-file-path))
               (read-file-name "tag file: " root nil nil "tags")))))
  (let* ((default-directory (ymacs-editor//project-root))
         (project (project-current))
         (files (when (and ymacs-universal-catgs-smart-git-integration-p
                           (file-directory-p ".git"))
                  (thread-first "git ls-files  --full-name --"
                    (shell-command-to-string)
                    (split-string "\n" :omit-nulls "\n\t ")))))

    (run-compilation!
     :name "Generate ctags"
     :command (ymacs-editor//build-ctags-command files -tag-file)
     :callback (lambda (&rest_)
                 (dolist (buffer (project--buffer-list project))
                   (with-current-buffer buffer
                     (when (derived-mode-p 'prog-mode)
                       (citre-auto-enable-citre-mode))))))))
