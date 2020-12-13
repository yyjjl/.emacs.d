;; -*- lexical-binding: t; -*-

(defvar comp-valid-source-re)
(declare-function comp-el-to-eln-filename "comp")
(declare-function native-compile "comp")
(declare-function native-comp-available-p "comp")

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
                     (directory (expand-file-name name ymacs-config-directory)))
                (when (file-directory-p directory)
                  directory))))
           (cl-delete nil)
           (seq-mapcat
            (lambda (dir)
              (directory-files dir t pattern)))))
         (failed-count 0))
    (with-current-buffer (find-file-noselect ymacs-autoloads-file)
      (erase-buffer)
      (dolist-with-progress-reporter (file files) "Generating autoloads ..."
        (condition-case err
            (generate-file-autoloads file)
          (user-error (message "Generating for %s failed: %s" file err)
                      (cl-incf failed-count))))
      (save-buffer))
    (message "%d generated, %d failed" (- (length files) failed-count) failed-count)))

;;;###autoload
(defun ymacs-package/compile-elpa-packages (&optional -no-message)
  (interactive)
  (let ((inhibit-message -no-message))
    (byte-recompile-directory package-user-dir nil t)))

;;;###autoload
(defun ymacs-package/compile-config (&optional -no-message)
  (interactive "P")
  (dolist-with-progress-reporter
      (file (cl-remove-duplicates
             (append
              (directory-files-recursively ymacs-private-directory "\\.el$")
              (cl-loop
               for file in  (directory-files ymacs-config-directory)
               when (and (not (file-directory-p file))
                         (string-suffix-p ".el" file))
               collect file)
              (cl-loop
               for feature in ymacs--loaded-features
               append (directory-files-recursively
                       (expand-file-name (symbol-name feature) ymacs-config-directory)
                       "\\.el$"))
              (list user-init-file
                    early-init-file
                    (expand-file-name "features.el" user-emacs-directory)
                    (expand-file-name "custom.el" user-emacs-directory)))))
      "Compiling configuration files ..."
    (when file
      (condition-case err
          (let ((inhibit-message -no-message))
            (byte-compile-file file)
            (when (fboundp 'native-compile)
              (native-compile file)))
        (error
         (when -no-message
           (message "%s: Error %s" file err)))))))
