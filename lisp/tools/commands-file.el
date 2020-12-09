;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-tools/copy-file-name (&optional -level -replace)
  "Copy current file name to king ring.
If ARG = 0 copy the current directory. If ARG > 0 copy the file
name without directory. If ARG < 0 copy the file name without
directory and extension."
  (interactive "p")
  (let ((path (or (buffer-file-name) default-directory)))
    (message "(-, +, 0) Level %d => %s"
             -level
             (kill-new (cl-case -level
                         (1 (buffer-name))
                         (2 path)
                         (3 (abbreviate-file-name path))
                         (4 default-directory)
                         (5 (file-name-nondirectory path))
                         (6 (file-name-base path)))
                       -replace)))
  (set-transient-map
   (define-key! :map (make-sparse-keymap)
     ("=" . (lambda! (ymacs-tools/copy-file-name (min 6 (1+ -level)) t)))
     ("-" . (lambda! (ymacs-tools/copy-file-name (max 1 (1- -level)) t)))
     ("0" . (lambda! (ymacs-tools/copy-file-name 1 t))))))


;;;###autoload
(defun ymacs-tools/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (user-error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (run-hooks 'ymacs-tools-after-delete-this-file-hook)
    (kill-this-buffer)))

;;;###autoload
(defun ymacs-tools/copy-this-file-to-new-file ()
  "Copy current file to a new file without close original file."
  (interactive)
  (let* ((this (current-buffer))
         (this-name (buffer-file-name))
         (name (completing-read "New file name: "
                                #'read-file-name-internal)))
    (if (and name this-name
             (string= name this-name)
             (not (get-buffer name)))
        (message "Copy failed !!!")
      (let ((buf (get-buffer-create name)))
        (with-current-buffer buf
          (insert-buffer-substring this)
          (write-file (expand-file-name name
                                        (file-name-directory this-name))))
        (switch-to-buffer buf)))))

;;;###autoload
(defun ymacs-tools/rename-this-file-and-buffer (-new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (list (let (confirm-nonexistent-file-or-buffer)
           (unless buffer-file-name
             (user-error "Current buffer is not visiting a file!"))
           (let ((new-name (completing-read "New file name: " #'read-file-name-internal)))
             (if (file-directory-p new-name)
                 (expand-file-name (file-name-nondirectory buffer-file-name) new-name)
               new-name)))))
  (let ((filename (buffer-file-name)))
    (when (or (not (file-exists-p -new-name))
              (not (equal filename -new-name))
              (yes-or-no-p (format "'%s' exists, overwrite it? " -new-name)))
      (set-visited-file-name -new-name)
      (rename-file filename -new-name)
      (let ((inhibit-message t))
        (recentf-cleanup))
      (run-hook-with-args 'ymacs-tools-after-rename-this-file-hook filename -new-name)
      (message "Rename to %s" -new-name))))
