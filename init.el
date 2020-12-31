;; -*- lexical-binding:t -*-

(if ymacs-dump-load-path
    (progn
      (setq load-path ymacs-dump-load-path)
      (with-current-buffer "*scratch*"
        (lispy-mode 1)))

  (require 'core-lib)

  (load-feature! default)

  (load-file! "custom")
  (load-file! "features"))
