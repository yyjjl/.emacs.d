;; -*- lexical-binding:t -*-

(require 'cl-lib)
(require 'core-lib)

(load-feature! default)

(load-file! "custom")

(load-feature! package)
(advice-add 'request! :override #'ignore)

(load-feature! ui)
(load-feature! completion-ivy)
(load-feature! tools)
(load-feature! hydra)

(recentf-mode 1)
(savehist-mode 1)
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

(ymacs-package|after-init)

(with-current-buffer (get-buffer-create "*scratch*")
  (erase-buffer)
  (insert (concat ";; Welcome to Emacs " (or user-login-name "") " !!!"))
  (special-mode)
  (setq cursor-type nil))

(setq ivy-read-action-function #'ivy-read-action-ivy)

(setq-default mode-line-format nil)
(setq-default window-min-height 1)
