;; -*- lexical-binding:t -*-

(when (< emacs-major-version 27)
  (load (expand-file-name "early-init" user-emacs-directory)))

(require 'core-lib)

(load-feature! default)

(ymacs//try-load-file custom-file (expand-etc! "custom-template.el"))

(load-feature! package)
(load-feature! ui)

(load-feature! popup)
(load-feature! completion_ivy)
(load-feature! company)
(load-feature! term)
(load-feature! misc)
(load-feature! hydra)
(load-feature! semantic)
(load-feature! hideshow)
(load-feature! edit)
(load-feature! dired)
(load-feature! ibuffer)

(ymacs//try-load-file features-file (expand-etc! "features-template.el"))
