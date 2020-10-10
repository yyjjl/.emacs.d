;;; -*- lexical-binding: t; -*-

(defun ymacs-semantic//db-parse-directory (-dir &optional -regex -recursive-p)
  (without-user-record!
   (let ((dir (file-name-as-directory -dir)))
     (dolist (file (directory-files dir))
       (unless (or (member file '("." ".."))
                   (member file ymacs-ignored-directories))
         (let ((full-file (expand-file-name file dir)))
           (with-demoted-errors "Error: %s"
             (if (and -recursive-p (file-directory-p full-file))
                 (ymacs-semantic//db-parse-directory full-file -regex -recursive-p)
               (if (or (not -regex)
                       (string= -regex "")
                       (string-match-p -regex file))
                   (save-excursion
                     (semanticdb-file-table-object full-file)))))))))))

;;;###autoload
(defun ymacs-semantic/db-parse (-regex -dir)
  (interactive
   (list (read-regexp "File name regex (default nil)" nil)
         (file-name-directory
          (completing-read "Directory: " #'read-file-name-internal))))
  (unwind-protect
      (progn
        (ymacs-semantic//db-parse-directory -dir -regex t)
        (semanticdb-cleanup-cache-files))
    (let ((inhibit-quit t))
      (semanticdb-save-all-db))
    (when-let (buffer (get-buffer "*Semanticdb Delete*"))
      (kill-buffer buffer))))
