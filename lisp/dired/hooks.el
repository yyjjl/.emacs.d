;;; -*- lexical-binding: t; -*-

(after! dired
  (define-hook! ymacs-dired|setup (dired-mode-hook)
    (setq mode-line-buffer-identification '("%b" (dired-omit-mode " (omit)")))
    (dired-hide-details-mode 1)))
