;;; -*- lexical-binding: t; -*-

(autoload #'dired-quick-sort-set-switches "dired-quick-sort")

(after! dired
  (define-hook! ymacs-dired|setup (dired-mode-hook)
    (when (or (not sys/macp) ymacs-gls-path)
      (dired-quick-sort-set-switches))
    (setq mode-line-buffer-identification '("%b" (dired-omit-mode " (omit)")))
    (dired-hide-details-mode 1)))
