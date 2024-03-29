;; -*- lexical-binding:t -*-

(require 'cl-lib)

(setq comp-deferred-compilation nil)

(defvar ymacs-setup-directory
  (expand-file-name "emacs" (file-name-directory load-file-name)))

(define-advice message (:around (-fn -fmt &rest -args) indent)
  (unless (string-prefix-p "Checking " -fmt)
    (apply -fn (concat "= " -fmt) -args)))

(add-to-list 'load-path ymacs-setup-directory)

(message "User emacs directory: %s" (abbreviate-file-name user-emacs-directory))
(message "Setup files directory: %s" (abbreviate-file-name ymacs-setup-directory))

(with-demoted-errors "%s"
  (setq load-prefer-newer t)
  (setq ymacs--buffer-visible-p nil)

  (setq early-init-file (expand-file-name "early-init.el" user-emacs-directory))
  (when (file-exists-p early-init-file)
    (load early-init-file))

  (setq package-quickstart-file (expand-file-name ".cache/quickstart.el" user-emacs-directory))
  (when (file-exists-p package-quickstart-file)
    (delete-file package-quickstart-file))

  (let ((elc-files (append (directory-files-recursively ymacs-config-directory "\\.elc$")
                           (directory-files user-emacs-directory t "\\.elc$")
                           (directory-files ymacs-cache-direcotry t "\\.elc$"))))
    (dolist-with-progress-reporter (elc-file elc-files)
        (format "Remove *.elc in %s ..." (abbreviate-file-name ymacs-config-directory))
      (delete-file elc-file)))

  (setq user-init-file (expand-file-name "init.el" user-emacs-directory))
  (load user-init-file)

  ;; Disable some features when load emacs
  (setq-default prog-mode-hook nil)
  (setq-default auto-mode-alist nil)
  (setq enable-local-variables :all)

  (when (file-exists-p ymacs-autoloads-file)
    (delete-file ymacs-autoloads-file))

  (run-hooks 'after-init-hook)

  (setq kill-emacs-hook nil)
  (setq ymacss-compile-config-in-progress t)

  (ymacs-default/compile-config)
  (ymacs-default/compile-elpa-packages)

  (package-quickstart-refresh))
