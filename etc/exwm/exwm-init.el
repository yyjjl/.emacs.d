(defvar emacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")

(defvar emacs-var-direcotry
  (expand-file-name "var/exwm" user-emacs-directory)
  "All data and external executable file in this direcotry")

;; Add `emacs-config-directory' to `load-path'
(add-to-list 'load-path emacs-config-directory)

;; All packages required in this section are defined in `init-packages'
(require 'core-lib)
;; Set some important variables

(require 'core-vars)
(require 'core-defaults)
;; (package-initialize)
;; Load core packages
(require 'core-packages-lib)
(require 'core-ui)
(require 'core-packages)

(require! 'exwm)

;; ----------------------------------------
;; Optional packages
;; ----------------------------------------

(require 'init-lisp)
(require 'init-ibuffer)
(require 'init-windows)

;; Other small tools
(require 'init-extra)

(core|after-init-hook)
;; (package|idle-init-emacs)

(recentf-mode 1)
(session-initialize)
(winner-mode 1)
(ivy-mode 1)
(counsel-mode 1)
;; (yas-global-mode 1)
(shackle-mode 1)

(global-eldoc-mode -1)

(remove-hook 'after-init-idle-hook 'package|idle-init-emacs)
(run-hooks 'after-init-idle-hook)

;; (window-numbering-mode 1)
(setq-default mode-line-format mode-line-default-format)
(setq-default mode-line-buffer-identification '("%b"))
(setq-default mode-line-misc-info nil)

(provide 'exwm-init)
