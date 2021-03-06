;; -*- lexical-binding:t -*-

(load (expand-file-name "early-init" user-emacs-directory))
(load (expand-file-name "lisp/core-lib" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load (file-name-sans-extension custom-file))
(setq org-export-async-debug nil)
