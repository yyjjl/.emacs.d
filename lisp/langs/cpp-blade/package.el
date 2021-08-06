;;; -*- lexical-binding: t; -*-

(autoload #'f-same-p "f")

(option! cpp-blade-build-name "build64_release"
  "build name"
  :type 'string
  :safe #'stringp)

(option! cpp-blade-project-name nil
  "build name"
  :type 'string
  :safe #'stringp)

(defvar-local ymacs-cpp-blade-root-directory nil
  "Project root for blade")

(defvar ymacs-cpp-blade-keymap
  (define-key! :map (make-sparse-keymap)
    ("C-c C-c" . ymacs-cpp-blade/clean-or-build-project)
    ([f9] . ymacs-cpp-blade/clean-or-build-project)))

(define-minor-mode ymacs-cpp-blade-mode
  "Use blade build-system"
  :group 'blade
  :global nil
  :keymap ymacs-cpp-blade-keymap)

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
              (unless (or (not (file-symlink-p link-path))
                          (f-same-p link-path cdb-path)
                          (not (yes-or-no-p (format "Link compile_commands.json for %s" ymacs-cpp-blade-build-name))))
                (delete-file link-path)
                (make-symbolic-link cdb-path link-path)))))
        t)))
  :lsp-enable-handler
  #'ymacs-cpp-blade-mode))
