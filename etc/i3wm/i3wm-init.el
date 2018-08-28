;; All packages required in this section are defined in `init-packages'
(require 'core-lib)
;; Set some important variables
(require 'core-vars)
(require 'core-env-vars)
(require 'core-defaults)
(require 'core-packages-lib)

(fset 'require! 'ignore)

(require 'core-ivy)
(require 'core-misc)
(require 'core-ui)

(eval-when-compile
  (require 'cl))
(require 'dash)
(require 'json)

(recentf-mode 1)
(session-initialize)
(winner-mode 1)
(ivy-mode 1)
(counsel-mode 1)
;; global-modes
(global-subword-mode 1)
(show-paren-mode 1)
;; Auto insert closing pair
(electric-pair-mode 1)
(electric-layout-mode 1)
(electric-indent-mode 1)

(with-current-buffer (get-buffer-create "*scratch*")
  (erase-buffer)
  (insert (concat ";; Welcome to Emacs " (or user-login-name "") " !!!"))
  (special-mode)
  (setq cursor-type nil))

(setq-default mode-line-format nil)
(setq-default window-min-height 1)

(provide 'i3wm-init)
