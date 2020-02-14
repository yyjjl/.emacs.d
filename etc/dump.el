(require 'package)

(load (expand-file-name "early-init" user-emacs-directory))
;; (load (expand-file-name "init.el" user-emacs-directory) nil :no-message t)

;; load autoload files and populate load-path’s
(package-initialize)
;; store load-path
(setq core-dumped-load-path load-path)
(setq core-dumped t)

;; (package-initialize) doens’t require each package, we need to load
;; those we want manually
(dolist (pkg package-selected-packages)
  (require pkg nil :noerror))

;; pre-load themes
(load-theme 'doom-molokai t t)
;; dump image
(dump-emacs-portable (expand-file-name "emacs.pdmp" emacs-var-direcotry))
