;;; -*- lexical-binding: t; -*-

(defun core//semanticdb-parse-directory (-dir &optional -regex -recursive-p)
  (without-user-record!
   (let ((dir (file-name-as-directory -dir)))
     (dolist (file (directory-files dir))
       (unless (or (member file '("." ".."))
                   (member file core-ignored-directories))
         (let ((full-file (expand-file-name file dir)))
           (condition-case err
               (if (and -recursive-p (file-directory-p full-file))
                   (core//semanticdb-parse-directory full-file -regex -recursive-p)
                 (if (or (not -regex)
                         (string= -regex "")
                         (string-match-p -regex file))
                     (save-excursion
                       (semanticdb-file-table-object full-file))))
             (error (message "Error: %s" err)))))))))

;;;###autoload
(defun core/semantidb-parse (-regex -dir)
  (interactive (list (read-regexp "File name regex (default nil)" nil)
                     (file-name-directory
                      (completing-read "Directory: "
                                       #'read-file-name-internal))))
  (unwind-protect
      (progn
        (core//semanticdb-parse-directory -dir -regex t)
        (semanticdb-cleanup-cache-files))
    (let ((inhibit-quit t))
      (semanticdb-save-all-db))
    (when-let (buffer (get-buffer "*Semanticdb Delete*"))
      (kill-buffer buffer))))
