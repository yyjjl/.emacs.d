;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-cpp-blade/clean-or-build-project (-do-build)
  (interactive "P")
  (unless ymacs-cpp-blade-root-directory
    (user-error "not in a blade project"))

  (let ((default-directory ymacs-cpp-blade-root-directory))
    (unless (and ymacs-cpp-blade-project-name
                 (file-directory-p ymacs-cpp-blade-project-name))
      (user-error "`ymacs-cpp-blade-project-name' is not set"))

    (let ((default-directory (expand-file-name ymacs-cpp-blade-project-name)))
      (run-compilation!
       :name (format "blade %s" ymacs-cpp-blade-project-name)
       :command (concat
                 (when -do-build
                   (concat (if (s-contains-p "release" ymacs-cpp-blade-build-name)
                               "./build.sh"
                             "./build_dev.sh")
                           " && "))
                 (format "python3 %s .." (expand-etc! "scripts/clean_cdb.py")))))))
