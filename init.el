;; -*- lexical-binding:t -*-

(require 'core-lib)

(defun load-core! ()
  (load-feature! core/default)
  (load-feature! core/ui)
  (load-feature! core/popup)
  (load-feature! core/editor)
  (load-feature! core/term)
  (load-feature! core/debug))

(if ymacs-dump-load-path
    (setq load-path ymacs-dump-load-path)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (unless (file-exists-p custom-file)
    (let ((template-file (expand-etc! "custom-template.el")))
      (unless (file-exists-p template-file)
        (user-error "Template %s does't exist" template-file))
      (copy-file template-file custom-file)))
  (load (file-name-sans-extension custom-file)))
