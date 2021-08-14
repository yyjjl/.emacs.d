;;; -*- lexical-binding: t; -*-

(defvar comp-valid-source-re)
(declare-function native-compile "comp")
(declare-function native-comp-available-p "comp")

;;;###autoload
(defun ymacs-default/generate-autoloads ()
  (interactive)
  (require 'autoload)

  (let ((pattern "\\`commands\\(-[^\\.]*\\)?\\.el\\'")
        (success-count 0)
        (failed-count 0))
    (with-current-buffer (find-file-noselect ymacs-autoloads-file)
      (erase-buffer)
      (cl-loop
       for (_ . feature) in ymacs-loaded-features
       for directory = (expand-file-name (symbol-name feature) ymacs-config-directory)
       when (file-directory-p directory)
       do (dolist (file (directory-files directory t pattern))
            (condition-case nil
                (progn
                  (generate-file-autoloads file)
                  (cl-incf success-count))
              (user-error
               (cl-incf failed-count)))))
      (save-buffer))
    (print! "> Autoload for %d files generated, %d files failed\n" success-count failed-count)

    (when (called-interactively-p 'interactive)
      (load ymacs-autoloads-file))))

;;;###autoload
(defun ymacs-default/compile-elpa-packages (&optional -no-message)
  (interactive)
  (let ((inhibit-message -no-message))
    (byte-recompile-directory package-user-dir 0)))

;;;###autoload
(defun ymacs-default/compile-config ()
  (interactive)
  (dolist (file (cl-remove-duplicates
                 (append
                  (when (file-exists-p ymacs-site-lisp-directory)
                    (directory-files-recursively ymacs-site-lisp-directory "\\.el$"))
                  (directory-files ymacs-config-directory t "\\.el$")
                  (directory-files user-emacs-directory t "\\.el$")
                  (cl-loop
                   for (_ . feature) in ymacs-loaded-features
                   for directory = (expand-file-name (symbol-name feature) ymacs-config-directory)
                   nconc (directory-files-recursively directory "\\.el$")))))
    (condition-case err
        (progn
          (print! "> Compiling %s\n" file)
          (byte-compile-file file)
          (when (native-comp-available-p)
            (native-compile file)))
      (error
       (print! "  > Failed %s\n" err)))))
