;; -*- lexical-binding:t -*-

(require 'core-lib)

(load-feature! default)
(ymacs//try-load-file custom-file (expand-etc! "custom-template.el"))
(load-feature! package)
(load-feature! ui)

(advice-add 'request! :override #'ignore)

(load-feature! completion_ivy)
(load-feature! misc)

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

(setq ivy-read-action-function #'ivy-read-action-ivy)

(setq-default mode-line-format nil)
(setq-default window-min-height 1)

(provide 'i3wm-init)
