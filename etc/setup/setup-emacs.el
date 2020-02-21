;; Setup Emacs environment
(require 'cl-lib)

(defvar emacs-setup-directory
  (expand-file-name "emacs" (file-name-directory load-file-name)))

(add-to-list 'load-path emacs-setup-directory)
(message "[Info] User emacs directory: %s"
         (abbreviate-file-name user-emacs-directory))

(message "[Info] Setup files directory: %s"
         (abbreviate-file-name emacs-setup-directory))

;; Disable some features when load emacs
(setq core--buffer-useful-p nil)
(with-demoted-errors "%s"
  (setq load-prefer-newer t)

  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early-init-file)
      (load early-init-file nil :no-message t)))
  (load (expand-file-name "init.el" user-emacs-directory) nil :no-message t)

  (setq-default prog-mode-hook nil)
  (setq-default auto-mode-alist nil)
  (setq enable-local-variables :all)

  (message "Remove *.elc in %s ..." (abbreviate-file-name emacs-config-directory))
  (dolist (elc-file (directory-files-recursively emacs-config-directory "\\.elc$")
                    (directory-files user-emacs-directory t "\\.elc$"))
    (delete-file elc-file))
  ;; Compile all configurations
  (package/generate-autoloads)
  (package/compile-config :nomessage))
