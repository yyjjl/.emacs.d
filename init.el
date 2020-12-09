;; -*- lexical-binding:t -*-

(cl-assert (>= emacs-major-version 27))

(require 'core-lib)

(load-feature! default)

(load-file! "custom")

(load-feature! package)
(load-feature! ui)
(load-feature! popup)
(load-feature! completion-ivy)
(load-feature! hydra)
(load-feature! company)
(load-feature! checker)
(load-feature! tools)
(load-feature! term)
(load-feature! semantic)
(load-feature! hideshow)
(load-feature! edit)
(load-feature! dired)
(load-feature! ibuffer)
(load-feature! lsp)
(load-feature! realgud)

;; Programming modes
(load-feature! lisp)
(load-feature! sh)

(load-file! "features")
