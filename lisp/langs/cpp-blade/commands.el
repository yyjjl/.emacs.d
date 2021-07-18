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
         (clangd-args
          (cons
           (format "--compile-commands-dir=%s" project-root)
           (cl-remove-if
            (lambda (x) (string-prefix-p "--compile-commands-dir" x))
            lsp-clients-clangd-args))))
    (add-dir-local-variable nil 'ymacs-default-project (cons 'local project-root))
    (add-dir-local-variable nil 'ymacs-cpp-blade-project-name project-name)
    (add-dir-local-variable nil 'lsp-clients-clangd-args clangd-args)
    (save-buffer)
    (hack-dir-local-variables-for-project!)))
