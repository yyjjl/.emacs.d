;; -*- lexical-binding: t; -*-

(define-hook! ymacs-package|after-init (after-init-hook)
  (setq package-selected-packages
        (hash-table-keys ymacs-package--required-packages))

  (unless (file-exists-p ymacs-autoloads-file)
    (autoload 'ymacs-package/generate-autoloads "package/commands" nil t)
    (ymacs-package/generate-autoloads))

  (load ymacs-autoloads-file))

(after! package
  (define-advice package--save-selected-packages (:override (-value) dont-save)
    (when -value
      (setq package-selected-packages -value))

    (unless after-init-time
      (add-hook 'after-init-hook #'package--save-selected-packages)))

  (define-advice package-generate-autoloads (:after (-name -pkg-dir) autoclose)
    "Auto close *-autoloads.el after a package installed."
    (when-let (buffer (find-file-existing
                       (expand-file-name (concat (if (symbolp -name)
                                                     (symbol-name -name)
                                                   -name)
                                                 "-autoloads.el")
                                         -pkg-dir)))
      (kill-buffer buffer)))

  (define-advice package-read-all-archive-contents (:after () set-archive)
    (dolist (pkg package-archive-contents)
      (ymacs-package//set-archive pkg (gethash (car pkg) ymacs-package--required-packages)))))
