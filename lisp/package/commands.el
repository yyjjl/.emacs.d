;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-package/generate-autoloads ()
  (interactive)
  (require 'autoload)
  (let ((files (cl-remove-duplicates
                (directory-files-recursively ymacs-config-directory "\\`commands\\(-[^\\.]*\\)?\\.el\\'")))
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

(defun ymacs-package/compile-elpa-packages (&optional -no-message?)
  (interactive)
  (let ((inhibit-message -no-message?))
    (byte-recompile-directory package-user-dir nil :force)))

(defun ymacs-package/compile-config (&optional -no-message?)
  (interactive "P")
  (message "Compile configuration files ...")
  (dolist (file (cl-remove-duplicates
                 (append
                  (directory-files-recursively ymacs-private-directory "\\.el$")
                  (directory-files-recursively ymacs-config-directory "\\.el$")
                  (directory-files user-emacs-directory :full "\\.el$"))))
    (when file
      (condition-case err
          (let ((inhibit-message -no-message?))
            (byte-compile-file file))
        (error (message "Error: %s" err)
               (backtrace)))))
  (message "Compile finished"))
