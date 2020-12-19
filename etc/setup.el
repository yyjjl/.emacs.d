;; -*- lexical-binding:t -*-

(require 'cl-lib)

(defvar ymacs-setup-directory
  (expand-file-name "emacs" (file-name-directory load-file-name)))

(add-to-list 'load-path ymacs-setup-directory)
(message "[Info] User emacs directory: %s"
         (abbreviate-file-name user-emacs-directory))

(message "[Info] Setup files directory: %s"
         (abbreviate-file-name ymacs-setup-directory))

(with-demoted-errors "%s"
  (setq load-prefer-newer t)
  (setq ymacs--buffer-visible-p nil)

  (setq early-init-file (expand-file-name "early-init.el" user-emacs-directory))
  (when (file-exists-p early-init-file)
    (load early-init-file nil t))

  (setq package-quickstart-file (expand-file-name ".cache/quickstart.el" user-emacs-directory))
  (when (file-exists-p package-quickstart-file)
    (delete-file package-quickstart-file))

  (let ((elc-files (append (directory-files-recursively ymacs-config-directory "\\.elc$")
                           (directory-files user-emacs-directory t "\\.elc$"))))
    (dolist-with-progress-reporter (elc-file elc-files)
        (format "Remove *.elc in %s ..." (abbreviate-file-name ymacs-config-directory))
      (delete-file elc-file)))

  (load (expand-file-name "init.el" user-emacs-directory) nil :no-message t)

  ;; Disable some features when load emacs
  (setq-default prog-mode-hook nil)
  (setq-default auto-mode-alist nil)
  (setq kill-emacs-hook nil)
  (setq after-init-hook nil)
  (setq enable-local-variables :all)

  (when (file-exists-p ymacs-autoloads-file)
    (delete-file ymacs-autoloads-file))

  (ymacs-package|after-init)

  (if (getenv "NATIVE_COMPILE_ELPA")
      (ymacs-package/native-compile-elpa-packages)

    (ymacs-package/compile-config)
    (ymacs-package/compile-elpa-packages :no-message)))
