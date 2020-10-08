;; -*- lexical-binding:t -*-

(when (< emacs-major-version 27)
  (load (expand-file-name "early-init" user-emacs-directory)))

(require 'core-lib)

(load-feature! default)

(load-file! "custom")

(load-feature! package)
(load-feature! ui)
(load-feature! popup)
(load-feature! completion-ivy)
(load-feature! misc)
(load-feature! hydra)
(load-feature! term)

(load-file! "features")
