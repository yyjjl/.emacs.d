;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-default/copy-file-name ()
  "Copy current file name to king ring."
  (interactive)
  (let ((path (or (buffer-file-name) default-directory)))
    (kill-new
     (completing-read-simple!
      :-prompt "Copy"
      :-collection (list
                    (buffer-name)
                    path
                    (abbreviate-file-name path)
                    default-directory
                    (file-name-nondirectory path)
                    (file-name-base path))))))


;;;###autoload
(defun ymacs-default/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (barf-if-not-visiting-file!)

  (let ((this-file (buffer-file-name)))
    (when (yes-or-no-p (format "Are you sure you want to delete `%s'?" this-file))
      (if (vc-backend this-file)
          (vc-delete-file this-file)
        (delete-file (buffer-file-name)))
      (kill-buffer))))

;;;###autoload
(defun ymacs-default/copy-this-file (-visit)
  "Copy current file to a new file without close original file."
  (interactive "P")
  (barf-if-not-visiting-file!)

  (let* ((this-file (buffer-file-name))
         (new-file (read-file-name "(Copy) New file: ")))

    (when (file-directory-p new-file)
      (setq new-file (expand-file-name (file-name-nondirectory this-file) new-file)))

    (when (equal this-file new-file)
      (user-error "Can not copy to current file"))

    (when (and (yes-or-no-p (format "Copy `%s' to `%s'" this-file new-file))
               (or (not (file-exists-p new-file))
                   (yes-or-no-p (format "%s already exists, overwrite? " new-file))))
      (make-directory (file-name-directory new-file) t)

      (copy-file this-file new-file nil t t t)
      (message "Wrote %s" new-file)
      (when -visit
        (find-file-other-window new-file)))))

;;;###autoload
(defun ymacs-default/rename-this-file-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let ((old-file (buffer-file-name)))
    (if (not old-file)
        (->> (buffer-name)
             (read-from-minibuffer "No file is associated with current buffer\nNew name: ")
             (rename-buffer))

      (unless (file-exists-p old-file)
        (user-error "`%s' is not saved yet" old-file))

      (let ((new-file (read-file-name "(Rename) New file: ")))
        (when (file-directory-p new-file)
          (setq new-file (expand-file-name (file-name-nondirectory old-file) new-file)))

        (when (equal old-file new-file)
          (user-error "Can not rename to current file"))

        (when (file-exists-p new-file)
          (user-error "Target `%s' exists" new-file))

        (when (yes-or-no-p (format "Rename `%s' to `%s'" old-file new-file))
          (make-directory (file-name-directory new-file) t)

          (if (vc-backend old-file)
              (vc-rename-file old-file new-file)
            (rename-file old-file new-file t)
            (set-visited-file-name new-file t t))

          (let ((inhibit-message t))
            (recentf-track-opened-file)))))))
