;; -*- lexical-binding:t -*-

;;;###autoload
(defun package/generate-autoloads ()
  (interactive)
  (let ((files (directory-files emacs-autoloads-directory :full "\\.el\\'"))
        (failed-count 0))
    (with-current-buffer (find-file-noselect emacs-autoloads-file)
      (erase-buffer)
      (dolist (file files)
        (condition-case nil
            (generate-file-autoloads file)
          (error (message "Generating for %s failed" file)
                 (cl-incf failed-count))))
      (save-buffer))
    (message "%d generated, %d failed"
             (- (length files) failed-count)
             failed-count)))

;;;###autoload
(defun package/compile-elpa-packages (&optional -no-message?)
  (interactive)
  (let ((inhibit-message -no-message?))
    (byte-recompile-directory package-user-dir nil :force)))

;;;###autoload
(defun package/compile-config (&optional -no-message?)
  (interactive "P")
  (message "Compile configuration files ...")
  (dolist (file (append
                 (directory-files emacs-config-directory :full "\\.el$")
                 (directory-files emacs-autoloads-directory :full "\\.el$")
                 (directory-files-recursively emacs-private-directory "\\.el$")
                 (directory-files user-emacs-directory :full "\\.el$")))
    (when file
      (condition-case err
          (let ((inhibit-message -no-message?))
            (byte-compile-file file))
        (error (message "Error: %s" err)
               (backtrace)))))
  (message "Compile finished"))
