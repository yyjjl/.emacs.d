;;; -*- lexical-binding: t; -*-

(after! ibuffer
  (define-hook! ymacs-ibuffer|setup (ibuffer-mode-hook)
    ;; (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")

    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))))
