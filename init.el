;; -*- lexical-binding:t -*-

(when (< emacs-major-version 27)
  (load (expand-file-name "early-init" user-emacs-directory)))

(require 'core-lib)

(load-feature! default)

(load-file! "custom")

(load-feature! package)
(load-feature! ui)
(load-feature! popup)
(load-feature! completion_ivy)
(load-feature! term)
(load-feature! misc)
(load-feature! hydra)
(load-feature! semantic)
(load-feature! hideshow)
(load-feature! edit)
(load-feature! dired)
(load-feature! ibuffer)

(load-file! "features")
