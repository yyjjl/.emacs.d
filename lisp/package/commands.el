;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-package/generate-autoloads ()
  (interactive)
  (require 'autoload)

  (let* ((pattern "\\`commands\\(-[^\\.]*\\)?\\.el\\'")
         (files
          (->>
           ymacs--loaded-features
           (mapcar
            (lambda (feature)
              (let* ((name (symbol-name feature))
                     (dir1 (expand-file-name name ymacs-config-directory))
                     (dir2 (expand-file-name name ymacs-extra-config-directory)))
                (or (when (file-directory-p dir1)
                      dir1)
                    (when (file-directory-p dir2)
                      dir2)))))
           (cl-delete nil)
           (seq-mapcat
            (lambda (dir)
              (directory-files dir t pattern)))))
         (failed-count 0))
    (with-current-buffer (find-file-noselect ymacs-autoloads-file)
      (erase-buffer)
      (dolist (file files)
        (condition-case err
            (generate-file-autoloads file)
          (user-error (message "Generating for %s failed: %s" file err)
                      (cl-incf failed-count))))
      (save-buffer))
    (message "%d generated, %d failed"
             (- (length files) failed-count)
             failed-count)))

;;;###autoload
(defun ymacs-package/compile-elpa-packages (&optional -no-message)
  (interactive)
  (let ((inhibit-message -no-message))
    (byte-recompile-directory package-user-dir nil t)))

;;;###autoload
(defun ymacs-package/native-compile-elpa-packages (&optional -no-message)
  (interactive)

  (when (ignore-errors (native-comp-available-p))
    (let* ((files
            (cl-loop
             for path in (list package-user-dir ymacs-private-directory)
             append
             (seq-filter
              (lambda (file)
                (not (or (string-suffix-p "-pkg.el" file)
                         (string-suffix-p "-autoloads.el" file)
                         (string-suffix-p "-theme.el" file)
                         (string-suffix-p ".dir-locals.el" file))))
              (directory-files-recursively path comp-valid-source-re))))
           (count 1))
      (dolist (file files)
        (let* ((out-filename (comp-el-to-eln-filename file))
               (out-dir (file-name-directory out-filename))
               (prompt (format "[%4d/%4d]" count (length files))))

          (cl-incf count)

          (unless (file-exists-p out-dir)
            (make-directory out-dir t))

          (unless (file-exists-p out-filename)
            (if (file-writable-p out-filename)
                (progn
                  (condition-case err
                      (progn
                        (message "%s native compiling %s" prompt file)
                        (let ((inhibit-message -no-message))
                          (native-compile file 'late))

                        (garbage-collect))
                    (error
                     (message "%s error: %s" prompt (error-message-string err))))
                  (when-let (memory (ymacs//show-process-memory))
                    (message "%s info: use %s%% RAM" prompt memory)
                    (when (> memory 70)
                      (user-error "%s error: use too much RAM" prompt))))
              (message "%s no write access for %s skipping." prompt out-filename))))))))

;;;###autoload
(defun ymacs-package/compile-config (&optional -no-message)
  (interactive "P")
  (message "Compile configuration files ...")
  (dolist (file (cl-remove-duplicates
                 (append
                  (directory-files-recursively ymacs-private-directory "\\.el$")
                  (directory-files-recursively ymacs-config-directory "\\.el$")
                  (directory-files user-emacs-directory t "\\.el$"))))
    (when file
      (condition-case err
          (let ((inhibit-message -no-message))
            (byte-compile-file file)
            (when (fboundp 'native-compile)
              (native-compile file 'late)))
        (error
         (when -no-message
           (message "%s: Error %s" file err))))))
  (message "Compile finished"))
