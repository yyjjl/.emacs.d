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
      (if -do-build
          (run-compilation!
           :name (format "blade %s" ymacs-cpp-blade-project-name)
           :command (concat
                     (when -do-build
                       (concat (if (s-contains-p "release" ymacs-cpp-blade-build-name)
                                   "./build.sh"
                                 "./build_dev.sh")
                               (unless (equal -do-build '(16))
                                 " -nu")))))
        (message "Use C-u to build")))))

;;;###autoload
(defun ymacs-cpp-blade/setup-project ()
  (interactive)
  (let* ((project-root (let ((dir (read-directory-name "Root: " (ymacs-editor//project-root))))
                         (unless (file-exists-p (expand-file-name "BLADE_ROOT" dir))
                           (user-error "BLADE_ROOT is not found"))
                         dir))
         (project-name
          (with-temp-lv-message! ("ROOT: %s" project-root)
            (read-string "Project name: " ymacs-cpp-blade-project-name)))
         (build-name (read-string "Build: " ymacs-cpp-blade-build-name)))
    (ymacs-editor//setup-project-internal
     project-root
     (list
      (cons 'ymacs-cpp-blade-project-name project-name)
      (cons 'ymacs-cpp-blade-build-name build-name)
      (cons 'ymacs-editor-search-directory (expand-file-name project-name project-root))))))
