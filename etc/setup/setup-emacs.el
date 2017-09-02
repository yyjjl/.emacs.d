;; Setup Emacs environment
(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(defvar emacs-setup-directory
  (expand-file-name "emacs" (file-name-directory load-file-name)))

(message "[Info] User emacs directory: %s"
         (abbreviate-file-name user-emacs-directory))
(message "[Info] Setup files directory: %s"
         (abbreviate-file-name emacs-setup-directory))

(toggle-debug-on-error)

(setq core--buffer-useful nil)
(load-file (expand-file-name "init.el" user-emacs-directory))
(setq-default prog-mode-hook nil)
(setq-default auto-mode-alist nil)
(setq enable-local-variables :all)

;; Compile all configurations
(core/compile-config t)

(add-to-list 'load-path emacs-setup-directory)
;; (dolist (file (directory-files emacs|setup-directory t))
;;   (when (and (not (file-directory-p file))
;;              (file-readable-p file))
;;     (load-file file)))

;; (kill-emacs)

;; TODO:
;; modes
