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
(defun ymacs-package/native-compile-elpa-packages (&optional -no-message)
  (interactive)

  (when (ignore-errors (native-comp-available-p))
    (let* ((files
            (cl-loop
             for path in (list package-user-dir ymacs-private-directory)
             append
             (seq-filter
              (lambda (file)
                (not
                 (string-match-p
                  (rx "-" (or "pkg" "autoloads" "theme" ".dir-locals") "el" string-end)
                  file)))
              (directory-files-recursively path comp-valid-source-re)))))
      (dolist-with-progress-reporter (file files) "Native compiling ..."
        (let* ((out-filename (comp-el-to-eln-filename file))
               (out-dir (file-name-directory out-filename)))

          (unless (file-exists-p out-dir)
            (make-directory out-dir t))

          (unless (file-exists-p out-filename)
            (if (file-writable-p out-filename)
                (progn
                  (condition-case err
                      (progn
                        (message "Native compiling %s" file)
                        (let ((inhibit-message -no-message))
                          (native-compile file 'late))
                        (garbage-collect))
                    (error
                     (message "Error: %s" (error-message-string err))))
                  (when-let (memory (ymacs//show-process-memory))
                    (message "Info: use %s%% RAM" memory)
                    (when (> memory 70)
                      (user-error "error: use too much RAM, please restart"))))
              (message "No write access for %s skipping." out-filename))))))))

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
