;;; -*- lexical-binding: t; -*-

(defvar comp-valid-source-re)
(declare-function comp-el-to-eln-filename "comp")
(declare-function native-compile "comp")
(declare-function native-comp-available-p "comp")

;;;###autoload
(defun ymacs-default/generate-autoloads ()
  (interactive)
  (require 'autoload)

  (let* ((pattern "\\`commands\\(-[^\\.]*\\)?\\.el\\'")
         (files
          (cl-loop
           for (_ . feature) in ymacs--loaded-features
           for directory = (expand-file-name (symbol-name feature) ymacs-config-directory)
           when (file-directory-p directory)
           nconc (directory-files directory t pattern)))
         (failed-count 0))
    (with-current-buffer (find-file-noselect ymacs-default-autoloads-file)
      (erase-buffer)
      (dolist-with-progress-reporter (file files) "Generating autoloads ..."
        (condition-case err
            (generate-file-autoloads file)
          (user-error (message "Generating for %s failed: %s" file err)
                      (cl-incf failed-count))))
      (save-buffer))
    (message "%d generated, %d failed" (- (length files) failed-count) failed-count)

    (when (called-interactively-p 'interactive)
      (load ymacs-default-autoloads-file))))

;;;###autoload
(defun ymacs-default/compile-elpa-packages (&optional -no-message)
  (interactive)
  (let ((inhibit-message -no-message))
    (byte-recompile-directory package-user-dir 0 t)))

;;;###autoload
(defun ymacs-default/compile-config (&optional -no-message)
  (interactive "P")
  (dolist-with-progress-reporter
      (file (cl-remove-duplicates
             (append
              (directory-files-recursively ymacs-private-directory "\\.el$")
              (directory-files ymacs-config-directory t "\\.el$")
              (directory-files user-emacs-directory t "\\.el$")
              (cl-loop
               for file in (directory-files ymacs-config-directory)
               when (and (not (file-directory-p file))
                         (string-suffix-p ".el" file))
               collect file)
              (cl-loop
               for (_ . feature) in ymacs--loaded-features
               for directory = (expand-file-name (symbol-name feature) ymacs-config-directory)
               nconc (directory-files-recursively directory "\\.el$")))))
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