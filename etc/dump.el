;; -*- lexical-binding:t -*-

(require 'package)

(load (expand-file-name "early-init" user-emacs-directory))

;; load autoload files and populate load-path’s
(package-initialize)

;; store load-path
(setq ymacs-dump-load-path load-path)
(setq ymacs-dump-p t)

(dolist (pkg package-selected-packages)
  (require pkg nil t))

;; pre-load themes
(load-theme 'doom-molokai t t)
;; dump image
(dump-emacs-portable (expand-file-name "emacs.pdmp" ymacs-cache-direcotry))
