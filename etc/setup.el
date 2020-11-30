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

  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early-init-file)
      (load early-init-file nil t)))
  (load (expand-file-name "init.el" user-emacs-directory) nil :no-message t)

  ;; Disable some features when load emacs
  (setq-default prog-mode-hook nil)
  (setq-default auto-mode-alist nil)
  (setq kill-emacs-hook nil)
  (setq after-init-hook nil)
  (setq enable-local-variables :all)

  (delete-file ymacs-autoloads-file)

  (ymacs-package|after-init)

  (ymacs-package/generate-autoloads)

  (if (getenv "NATIVE_COMPILE_ELPA")
      (ymacs-package/native-compile-elpa-packages)

    (message "Remove *.elc in %s ..." (abbreviate-file-name ymacs-config-directory))

    (delete-file (expand-var! "autoloads.el"))
    (dolist (elc-file (directory-files-recursively ymacs-config-directory "\\.elc$")
                      (directory-files user-emacs-directory t "\\.elc$"))
      (delete-file elc-file))

    (ymacs-package/compile-config)
    (ymacs-package/compile-elpa-packages :no-message)))
