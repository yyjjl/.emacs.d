;; -*- lexical-binding:t -*-

(load (expand-file-name "lisp/core-lib" user-emacs-directory) nil t)

(if ymacs-dump-load-path
    (setq load-path ymacs-dump-load-path)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (let ((custom-file-exists (file-exists-p custom-file)))
    (unless custom-file-exists
      (let ((template-file (expand-etc! "custom-template.el")))
        (unless (file-exists-p template-file)
          (user-error "Template %s does't exist" template-file))
        (copy-file template-file custom-file)))

    (load (file-name-sans-extension custom-file) nil t)

    (unless custom-file-exists
      (dolist (item (get 'ymacs 'custom-group))
        (when (memq 'custom-variable item)
          (let ((symbol (car item)))
            (unless (get symbol 'saved-value)
              (put symbol 'saved-value (symbol-value symbol))))))
      (print! "> Saving customizations...\n")
      (custom-save-all))))
