;; Setup Emacs environment
(require 'cl)
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
(condition-case err
    (progn
      (load (expand-file-name "init.el" user-emacs-directory)
            nil :no-message t)
      (setq-default prog-mode-hook nil)
      (setq-default auto-mode-alist nil)
      (setq enable-local-variables :all)

      (message "Remove *.elc in %s ..."
               (abbreviate-file-name emacs-config-directory))
      (dolist (elc-file (directory-files-recursively emacs-config-directory
                                                     "\\.elc$"))
        (delete-file elc-file))
      ;; Compile all configurations
      (package/compile-config :nomessage))
  (error (message "Error: %s" err)))
