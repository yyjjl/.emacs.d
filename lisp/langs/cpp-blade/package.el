;;; -*- lexical-binding: t; -*-

(autoload #'f-same-p "f")

(option! cpp-blade-build-name "build64_release"
  "build name"
  :type 'string
  :safe #'stringp)

(option! cpp-blade-root-directory nil
  "Project root for blade"
  :type 'string
  :safe #'stringp)

(add-to-list
 'ymacs-cpp-build-systems
 (make-ymacs-cpp-build-system
  :system-id 'blade
  :lsp-enable-fn
  (lambda ()
    (let ((project-root (ymacs-editor//project-root)))
      (when (and project-root
                 (setq ymacs-cpp-blade-root-directory (locate-topmost! "BLADE_ROOT")))
        (let ((cdb-path (expand-file-name
                         "compile_commands.json"
                         (expand-file-name ymacs-cpp-blade-build-name
                                           ymacs-cpp-blade-root-directory)))
              (link-path (expand-file-name "compile_commands.json" project-root)))
          (if (not (file-exists-p cdb-path))
              (message "Project is not built yet !!")
            (if (not (file-exists-p link-path))
                (make-symbolic-link cdb-path link-path)
              (unless (or (f-same-p link-path cdb-path)
                          (not (yes-or-no-p (format "Link compile_commands.json for %s" ymacs-cpp-blade-build-name))))
                (delete-file link-path)
                (make-symbolic-link cdb-path link-path)))))
        t)))))
