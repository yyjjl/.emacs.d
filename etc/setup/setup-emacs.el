;; Setup Emacs environment
(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(defvar emacs-setup-directory
  (expand-file-name "emacs" (file-name-directory load-file-name)))

(defvar log-file-name
  (expand-file-name "setup.log" (file-name-directory load-file-name)))

(defvar skip-confirm-default (member "-y" command-line-args-left))

(defun get-log-buffer ()
  (or (get-file-buffer log-file-name)
      (find-file-noselect log-file-name)))

(defun install-modules ()
  (message "Install modules ...")
  (let (succ-modules
        failed-modules
        skipped-modules
        module)
    (dolist (file (directory-files emacs-setup-directory t))
      (when (and (not (file-directory-p file))
                 (file-readable-p file))
        (setq module (file-name-base file))
        (condition-case err
            (if (or skip-confirm-default
                    (y-or-n-p (format "Install `%s'" module)))
                (progn
                  (message "Installing module `%s' ..." module)
                  (load file nil :no-message t)
                  (push module succ-modules))
              (push module skipped-modules))
          (error
           (message "Load module `%s' failed: %s" module err)
           (push module failed-modules)))))
    (message "[Info] Installed modules: %s" (or succ-modules "()"))
    (message "[Info] Failed modules: %s" (or failed-modules "()"))
    (message "[Info] Skipped modules: %s" (or skipped-modules "()"))))

(defun run-command ($program &rest $args)
  (message "[Command] %s %s" $program (string-join $args " "))
  (destructuring-bind (exit-code output)
      (with-temp-buffer
        (values
         (apply #'call-process $program nil (current-buffer) nil $args)
         (buffer-string)))
    (message "[ExitCode] %s" exit-code)
    (when (/= exit-code 0)
      (message "[Warning] Execution failed !!!")
      (with-current-buffer (get-log-buffer)
        (goto-char (point-max))
        (insert output "\n")
        (basic-save-buffer)))))

(add-to-list 'load-path emacs-setup-directory)
(message "[Info] User emacs directory: %s"
         (abbreviate-file-name user-emacs-directory))

(message "[Info] Setup files directory: %s"
         (abbreviate-file-name emacs-setup-directory))


;; Disable some features when load emacs
(setq core--buffer-useful nil)
(condition-case err
    (progn
      (load (expand-file-name "init.el" user-emacs-directory)
            nil :no-message t)
      (setq-default prog-mode-hook nil)
      (setq-default auto-mode-alist nil)
      (setq enable-local-variables :all)

      (if (eq system-type 'gnu/linux)
          (install-modules)
        (message "(%s) External tools won't be installed automatically"
                 system-type))

      (message "Remove *.elc in %s ..." (abbreviate-file-name emacs-config-directory))
      (dolist (elc-file (directory-files-recursively emacs-config-directory
                                                     "\\.elc$"))
        (delete-file elc-file))
      ;; Compile all configurations
      (core/compile-config :no-message))
  (error (message "Error: %s" err)))

;; (kill-emacs)

;; TODO:
;; modes
