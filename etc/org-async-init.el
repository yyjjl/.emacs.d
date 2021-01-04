;; -*- lexical-binding:t -*-

(load (expand-file-name "early-init" user-emacs-directory))

(require 'core-lib)

(defun load-core! ()
  (load-feature! core/default)
  (load-feature! core/ui)
  (load-feature! core/popup)
  (load-feature! core/editor)
  (load-feature! core/term)
  (load-feature! core/debug))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load (file-name-sans-extension custom-file))

(setq org-export-async-debug nil)
