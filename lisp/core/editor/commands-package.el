;;; -*- lexical-binding: t; -*-

(defvar comp-valid-source-re)
(declare-function comp-el-to-eln-filename "comp")
(declare-function native-compile "comp")
(declare-function native-comp-available-p "comp")

;;;###autoload
(defun ymacs-editor/generate-autoloads ()
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
(defun ymacs-editor/compile-elpa-packages (&optional -no-message)
  (interactive)
  (let ((inhibit-message -no-message))
    (byte-recompile-directory package-user-dir 0)))

;;;###autoload
(defun ymacs-editor/compile-config ()
  (interactive)
  (dolist (file (cl-remove-duplicates
                 (append
                  (directory-files-recursively ymacs-site-lisp-directory "\\.el$")
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
          (when (fboundp 'native-compile)
            (native-compile file)))
      (error
       (print! "  > Failed %s\n" err)))))
