;; -*- lexical-binding:t -*-

(defun ymacs//setup-emacs ()
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
  (load-feature! debug)
  (load-feature! term)
  (load-feature! semantic)
  (load-feature! hideshow)
  (load-feature! edit)
  (load-feature! dired)
  (load-feature! ibuffer)

  ;; Programming modes
  (load-feature! lisp)

  (load-file! "features"))

(if ymacs-dump-load-path
    (progn
      (setq load-path ymacs-dump-load-path)
      (with-current-buffer "*scratch*"
        (lispy-mode 1)))
  (ymacs//setup-emacs))
