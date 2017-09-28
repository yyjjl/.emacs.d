;; All packages required in this section are defined in `init-packages'
(require 'core-lib)
;; Set some important variables

(setq emacs-var-direcotry
      (expand-file-name "var/exwm/" user-emacs-directory))
(require 'core-vars)
(setq custom-file nil)

(require 'core-defaults)
(require 'core-ui)
;; (package-initialize)
;; Load core packages
(require 'core-packages)

;; ----------------------------------------
;; Optional packages
;; ----------------------------------------
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-lisp)

;; Other small tools
(require 'init-extra)

(core|after-init-hook)
(package|after-init-hook)

(recentf-mode 1)
(session-initialize)
(winner-mode 1)
(ivy-mode 1)
(counsel-mode 1)
(yas-global-mode 1)
(shackle-mode 1)

(window-numbering-mode 1)
(setq-default mode-line-format mode-line-default-format)
(setq-default mode-line-buffer-identification '("%b"))
(setq-default mode-line-misc-info
              '((global-mode-string ("" global-mode-string " "))
                (iedit-mode (:eval (format "Iedit:%d" (iedit-counter))))))


(provide 'exwm-init)
