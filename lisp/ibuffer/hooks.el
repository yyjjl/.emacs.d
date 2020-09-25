;;; -*- lexical-binding: t; -*-

(after! ibuffer
  (define-hook! ymacs-ibuffer|setup (ibuffer-mode-hook)
    (ibuffer-projectile-set-filter-groups)
    ;; (ibuffer-auto-mode 1)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))))
